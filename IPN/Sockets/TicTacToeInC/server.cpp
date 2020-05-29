#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

typedef int id;

struct game {
  static const char BLANK = ' ', PLAYER1 = 'O', PLAYER2 = 'X';

  char board[3][3] = {{BLANK, BLANK, BLANK}, {BLANK, BLANK, BLANK}, {BLANK, BLANK, BLANK}};
  id players[2];
};

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

  id listener;
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

int main() {
  const char* port = "9034";
  const char hello_message[] = "Welcome to tictactoe. You are now in the queue to find a match\n";

  const id listener = get_listener_socket(port);
  std::vector<pollfd> to_monitor {create_poll_data(listener)};
  std::unordered_set<id> waiting_for_parther;
  std::unordered_map<id, game*> games_being_play;
  printf("Server (port: %s) ready :D\n\n", port);

  const auto start_possible_games = [&]() {
    while (waiting_for_parther.size() > 1) {
      auto queue = begin(waiting_for_parther);
      id player1 = *queue;
      queue = waiting_for_parther.erase(queue);
      id player2 = *queue;
      queue = waiting_for_parther.erase(queue);

      struct game* new_game = (struct game*) malloc(sizeof(game));
      new_game->players[0] = player1, new_game->players[1] = player2;
      games_being_play[player1] = new_game;
      games_being_play[player2] = new_game;

      printf("New game started: %d vs %d", player1, player2);
      const char message[] = "Player found, Match started\n";
      if (send(player1, message, sizeof(message), 0) != sizeof(message))
        fprintf(stderr, "error sending greeting info to player 1\n");
      if (send(player2, message, sizeof(message), 0) != sizeof(message))
        fprintf(stderr, "error sending greeting info to player 2\n");
    }
  };

  const auto add_new_client = [&]() {
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
  };

  const auto handle_disconection = [&](const id sender, size_t& i) {
    close(sender);
    const auto it_waiting = waiting_for_parther.find(sender);
    if (it_waiting != end(waiting_for_parther)) waiting_for_parther.erase(it_waiting);

    const auto it_game = games_being_play.find(sender);
    if (it_game != end(games_being_play)) {
      const game* current_game = it_game->second;
      const id other =
          current_game->players[0] == sender ? current_game->players[1] : current_game->players[0];

      const char message[] = "Other player connection is lost. You are waiting for a new match\n";
      if (send(other, message, sizeof(message), 0) != sizeof(message))
        fprintf(stderr, "error sending restarting info to %d\n", other);
      if (send(other, hello_message, sizeof(hello_message), 0) != sizeof(hello_message))
        fprintf(stderr, "error sending greeting info to %d\n", other);

      waiting_for_parther.insert(other);
    }

    to_monitor[i--] = to_monitor.back();
    to_monitor.pop_back();
  };

  const auto handle_connection = [&](size_t i) {
    char buffer[1024];
    const id sender = to_monitor[i].fd;
    const ssize_t num_bytes_read = recv(sender, buffer, sizeof(buffer), 0);

    if (num_bytes_read == 2) fprintf(stderr, "empty message from %d\n", sender);
    else if (num_bytes_read <= 0) {
      if (num_bytes_read == 0) fprintf(stderr, "socket %d hung up, closing it\n", sender);
      if (num_bytes_read < 0) fprintf(stderr, "error reading %d, closing it\n", sender);
      handle_disconection(sender, i);
    } else {
      buffer[num_bytes_read - 2] = '\0';
      printf("Message from %d saying: %s\n", sender, buffer);
      buffer[num_bytes_read - 2] = '\n';
      buffer[num_bytes_read - 1] = '\0';

      const auto it_game = games_being_play.find(sender);
      if (it_game == end(games_being_play)) return;

      const game* current_game = it_game->second;
      const id destination =
          current_game->players[0] == sender ? current_game->players[1] : current_game->players[0];

      if (send(destination, buffer, num_bytes_read - 1, 0) != num_bytes_read - 1)
        fprintf(stderr, "error sending to %d\n", destination);
    }
  };

  while (true) {
    const int timeout_ms = 50;
    int events_to_check = poll(to_monitor.data(), to_monitor.size(), timeout_ms);
    if (events_to_check == -1) perror("Error at polling :/");

    for (size_t i = 0; i < to_monitor.size() and events_to_check > 0; ++i) {
      if (to_monitor[i].revents & POLLIN) {
        if (to_monitor[i].fd == listener) add_new_client();
        if (to_monitor[i].fd != listener) handle_connection(i);
        --events_to_check;
      }
    }

    start_possible_games();
  }

  return 0;
}
