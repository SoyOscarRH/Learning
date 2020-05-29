#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

typedef int id;

#include "./game.cpp"

id listener = -1;
std::vector<pollfd> to_monitor;
std::unordered_set<id> to_delete;
std::unordered_set<id> waiting_for_parther;
std::unordered_map<id, game*> games_being_play;
const char hello_message[] = "Welcome to tictactoe. You are now in the queue to find a match\n";

id get_listener_socket(const char* port_name) {
  struct addrinfo* possible_addresses;
  struct addrinfo* hints = (struct addrinfo*) calloc(1, sizeof(struct addrinfo));
  hints->ai_family = AF_UNSPEC;
  hints->ai_socktype = SOCK_STREAM;
  hints->ai_flags = AI_PASSIVE;

  const int error_code = getaddrinfo(NULL, port_name, hints, &possible_addresses);
  if (error_code != 0) {
    fprintf(stderr, "error setting up: %s\n", gai_strerror(error_code));
    exit(1);
  }

  int valid_option = 0;
  for (struct addrinfo* option = possible_addresses; option and !valid_option;
       option = option->ai_next) {
    listener = socket(option->ai_family, option->ai_socktype, option->ai_protocol);
    if (listener < 0) continue;

    int yes = 1;
    setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));

    if (bind(listener, option->ai_addr, option->ai_addrlen) != 0) close(listener);
    else
      valid_option = true;
  }

  freeaddrinfo(possible_addresses);
  if (not valid_option) {
    fprintf(stderr, "Error: I did not find any available ports\n");
    exit(1);
  }

  const int possible_incomming_connections = 10;
  if (listen(listener, possible_incomming_connections) == -1) {
    fprintf(stderr, "Error setting up a listen\n");
    exit(1);
  }

  return listener;
}

struct pollfd create_poll_data(const id fd) {
  struct pollfd poll_struct;
  poll_struct.fd = fd;
  poll_struct.events = POLLIN;
  return poll_struct;
}

void show_connection_details(struct sockaddr_storage* address, const id new_client) {
  int family = address->ss_family;
  void* real_address = family == AF_INET ? (void*) &(((struct sockaddr_in*) address)->sin_addr)
                                         : (void*) &(((struct sockaddr_in6*) address)->sin6_addr);

  char remoteIP[INET6_ADDRSTRLEN];
  const char* address_name = inet_ntop(family, real_address, remoteIP, INET6_ADDRSTRLEN);
  printf("New connection from %s is now on socket %d\n", address_name, new_client);
}

void start_possible_games() {
  while (waiting_for_parther.size() > 1) {
    auto queue = begin(waiting_for_parther);
    id player1 = *queue;
    queue = waiting_for_parther.erase(queue);
    id player2 = *queue;
    queue = waiting_for_parther.erase(queue);

    struct game* new_game = new game();
    new_game->players[0] = player1, new_game->players[1] = player2;

    games_being_play[player1] = new_game;
    games_being_play[player2] = new_game;
    new_game->is_turn_of = player1;
    new_game->name = games_being_play.size() / 2;

    const char message[] = "Player found, Match started\n";
    if (send(player1, message, sizeof(message), 0) != sizeof(message))
      fprintf(stderr, "error sending greeting info to player 1\n");
    if (send(player2, message, sizeof(message), 0) != sizeof(message))
      fprintf(stderr, "error sending greeting info to player 2\n");

    printf("New game started: %d vs %d\n[Game ID:%d]\n%s", player1, player2, new_game->name,
           new_game->board_display);

    const int size = sizeof(new_game->board_display);
    if (send(player1, new_game->board_display, size, 0) != size)
      fprintf(stderr, "error board to %d\n", player1);

    if (send(player2, new_game->board_display, size, 0) != size)
      fprintf(stderr, "error board to %d\n", player1);

    const char message_start[] = "Is your turn. Please select a cell: ";
    if (send(player1, message_start, sizeof(message_start), 0) != sizeof(message_start))
      fprintf(stderr, "error sending message_start to player 1\n");
  }
}

void add_new_client() {
  struct sockaddr_storage client_address;
  socklen_t addrlen = sizeof(client_address);
  const id new_client = accept(listener, (struct sockaddr*) &client_address, &addrlen);

  if (new_client == -1) {
    perror("error accepting connection");
    return;
  }

  if (send(new_client, hello_message, sizeof(hello_message), 0) != sizeof(hello_message))
    fprintf(stderr, "error sending greeting to %d\n", new_client);
  else {
    show_connection_details(&client_address, new_client);
    to_monitor.push_back(create_poll_data(new_client));
    waiting_for_parther.insert(new_client);
  }
}

void handle_disconection(const id sender) {
  const auto it_waiting = waiting_for_parther.find(sender);
  if (it_waiting != end(waiting_for_parther)) waiting_for_parther.erase(it_waiting);

  const auto it_game = games_being_play.find(sender);
  if (it_game != end(games_being_play)) {
    const game* current_game = it_game->second;
    const id other =
        current_game->players[0] == sender ? current_game->players[1] : current_game->players[0];

    delete (current_game);
    games_being_play.erase(games_being_play.find(sender));
    games_being_play.erase(games_being_play.find(other));

    const char message[] = "Other player connection is lost. You are waiting for a new match\n";
    if (send(other, message, sizeof(message), 0) != sizeof(message))
      fprintf(stderr, "error sending restarting info to %d\n", other);
    if (send(other, hello_message, sizeof(hello_message), 0) != sizeof(hello_message))
      fprintf(stderr, "error sending greeting info to %d\n", other);

    waiting_for_parther.insert(other);
  }

  to_delete.insert(sender);
  close(sender);
}

void handle_connection(const int sender) {
  char buffer[1024];
  const ssize_t num_bytes_read = recv(sender, buffer, sizeof(buffer), 0);

  const auto it_game = games_being_play.find(sender);
  if (it_game == end(games_being_play)) return;

  game* current_game = it_game->second;
  if (current_game->is_turn_of != sender) {
    const char message[] = "Relax is not your turn\n";
    if (send(sender, message, sizeof(message), 0) != sizeof(message))
      fprintf(stderr, "error sending waiting message to %d\n", sender);
    return;
  }

  if (num_bytes_read == 2) fprintf(stderr, "empty message from %d\n", sender);
  else if (num_bytes_read <= 0) {
    if (num_bytes_read == 0) fprintf(stderr, "socket %d hung up, closing it\n", sender);
    if (num_bytes_read < 0) fprintf(stderr, "error reading %d, closing it\n", sender);
    handle_disconection(sender);
  } else {
    const int move = buffer[0] - '0';
    if (num_bytes_read != 3 or !current_game->is_viable(move)) {
      const char message[] = "Not a valid move\n";
      if (send(sender, message, sizeof(message), 0) != sizeof(message))
        fprintf(stderr, "error sending wrong message to %d\n", sender);
      return;
    }

    const id destination =
        current_game->players[0] == sender ? current_game->players[1] : current_game->players[0];

    current_game->set(sender, move);
    printf("Move from %d\n[Game ID:%d]\n%s", sender, current_game->name,
           current_game->board_display);

    current_game->is_turn_of = destination;
    const int size = sizeof(current_game->board_display);
    if (send(destination, current_game->board_display, size, 0) != size)
      fprintf(stderr, "error board to %d\n", destination);
    if (send(sender, current_game->board_display, size, 0) != size)
      fprintf(stderr, "error board to %d\n", sender);

    const id winner = current_game->winner();
    if (winner != -1) {
      const char message[] = "We have a winner. Thanks for playing\nYou can alway connect back\n\n";
      if (send(destination, message, sizeof(message), 0) != sizeof(message))
        fprintf(stderr, "error winner to %d\n", destination);
      if (send(sender, message, sizeof(message), 0) != sizeof(message))
        fprintf(stderr, "error winner to %d\n", sender);

      to_delete.insert(destination);
      to_delete.insert(sender);

      games_being_play.erase(games_being_play.find(sender));
      games_being_play.erase(games_being_play.find(destination));

      close(sender);
      close(destination);

      delete (current_game);
    }
  }
}

void delete_fd() {
  const auto f = [&](const struct pollfd x) { return to_delete.find(x.fd) != to_delete.end(); };
  const auto it = std::remove_if(std::begin(to_monitor), std::end(to_monitor), f);
  to_monitor.erase(it, to_monitor.end());
  to_delete.clear();
}

int main() {
  const char* port = "9034";

  listener = get_listener_socket(port);
  to_monitor.push_back(create_poll_data(listener));

  printf("Server (port: %s) ready :D\n\n", port);

  while (true) {
    const int timeout_ms = 2500;
    int events_to_check = poll(to_monitor.data(), to_monitor.size(), timeout_ms);
    if (events_to_check == -1) perror("Error at polling :/");

    printf("Monitoring: ");
    for (const auto id : to_monitor) printf("[%d] ", id.fd);

    printf("\nGames active %lu\n\n", games_being_play.size() / 2);

    start_possible_games();

    for (const auto connection : to_monitor) {
      if (not events_to_check) break;

      if (connection.revents & POLLIN) {
        if (connection.fd == listener) add_new_client();
        if (connection.fd != listener) handle_connection(connection.fd);
        --events_to_check;
      }
    }

    delete_fd();
  }

  return 0;
}
