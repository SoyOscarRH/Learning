#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include <unordered_map>
#include <vector>

typedef int id;

void* get_in_addr(struct sockaddr* sa) {
  if (sa->sa_family == AF_INET) { return &(((struct sockaddr_in*)sa)->sin_addr); }

  return &(((struct sockaddr_in6*)sa)->sin6_addr);
}

id get_listener_socket(const char* port_name) {
  struct addrinfo* possible_addresses;
  struct addrinfo* hints = (struct addrinfo*)calloc(1, sizeof(struct addrinfo));
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
  for (struct addrinfo* option = possible_addresses; option and !valid_option; option = option->ai_next) {
    listener = socket(option->ai_family, option->ai_socktype, option->ai_protocol);
    if (listener < 0) continue;

    int yes = 1;
    setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));

    if (bind(listener, option->ai_addr, option->ai_addrlen) != 0) close(listener);
    else valid_option = true;
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

void add_new_connection(std::vector<struct pollfd>& to_monitor, const id listener) {
  struct sockaddr_storage client_address;
  socklen_t addrlen = sizeof(client_address);
  const id new_client = accept(listener, (struct sockaddr*)&client_address, &addrlen);

  if (new_client == -1) {
    perror("error accepting connection");
    return;
  }

  to_monitor.push_back(create_poll_data(new_client));

  char remoteIP[INET6_ADDRSTRLEN];

  void* correct_address = get_in_addr((struct sockaddr*)&client_address);
  int family = client_address.ss_family;
  const char* address_name = inet_ntop(family, correct_address, remoteIP, INET6_ADDRSTRLEN);
  printf("pollserver: new connection from %s on socket %d\n", address_name, new_client);
}

void handle_connection(std::vector<struct pollfd>& to_monitor, size_t& i, const id listener) {
  char buffer[1024];
  const id sender = to_monitor[i].fd;
  const ssize_t num_bytes_read = recv(sender, buffer, sizeof(buffer), 0);

  printf("Sending message to the chat: %s", buffer);

  if (num_bytes_read <= 0) {
    if (num_bytes_read == 0) fprintf(stderr, "socket %d hung up, closing it\n", sender);
    if (num_bytes_read < 0) fprintf(stderr, "error reading %d, closing it\n", sender);

    close(sender);
    to_monitor[i--] = to_monitor.back();
    to_monitor.pop_back();

    return;
  }

  for (auto& client : to_monitor) {
    const id destination = client.fd;

    if (destination == listener) continue;
    if (destination == sender) continue;

    const ssize_t num_bytes_sent = send(destination, buffer, num_bytes_read, 0);
    if (num_bytes_sent < 1) fprintf(stderr, "error sending to %d\n", destination);
  }
}

int main() {
  const id listener = get_listener_socket("9034");
  std::vector<struct pollfd> to_monitor {create_poll_data(listener)};

  while (true) {
    int events_to_check = poll(to_monitor.data(), to_monitor.size(), -1);
    if (events_to_check == -1) perror("Error at polling :/");

    for (size_t i = 0; i < to_monitor.size() and events_to_check > 0; ++i) {
      if (to_monitor[i].revents & POLLIN) {
        if (to_monitor[i].fd == listener) add_new_connection(to_monitor, listener);
        if (to_monitor[i].fd != listener) handle_connection(to_monitor, i, listener);
        --events_to_check;
      }
    }
  }

  return 0;
}
