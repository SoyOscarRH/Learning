// Servidor EcoTCPServidor.c
#include <arpa/inet.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include "./error.h"

const int max_waiting_connections = 5;
const size_t buffer_size = 1024;

void manage(int);

int main(int argc, char** argv) {
  if (argc != 2) show_final_message("Use: port");
  in_port_t port = atoi(argv[1]);

  const int socket_fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (socket_fd < 0) show_final_message("Error opening a socket_communication");

  struct sockaddr_in server;
  {
    memset(&server, 0, sizeof(server));
    server.sin_family = AF_INET;
    server.sin_port = htons(port);
    server.sin_addr.s_addr = htons(INADDR_ANY);
  }

  const struct sockaddr* generic_address = (struct sockaddr*)&server;
  if (bind(socket_fd, generic_address, sizeof(server)) < 0) show_final_message("Error at binding");
  if (listen(socket_fd, max_waiting_connections) < 0) show_final_message("Error at listening");

  while (1) {
    printf("Server ready for new connections :) ...\n");
    struct sockaddr_in client;

    socklen_t size_of_struct = sizeof(client);
    int socket_communication = accept(socket_fd, (struct sockaddr*)&client, &size_of_struct);
    if (socket_communication < 0) show_final_message("Fail at connection");

    char client_name[INET_ADDRSTRLEN];
    if (inet_ntop(AF_INET, &client.sin_addr.s_addr, client_name, sizeof(client_name)))
      printf("Client connected: %s:%d\n", client_name, ntohs(client.sin_port));
    else
      printf("Not possible to connect\n");

    manage(socket_communication);
  }
}

void manage(const int socket_communication) {
  char buffer[buffer_size];

  ssize_t num_bytes_received = recv(socket_communication, buffer, buffer_size, 0);
  if (num_bytes_received < 0) show_final_message("Failed reception");

  const ssize_t num_bytes_sent = send(socket_communication, buffer, num_bytes_received, 0);
  if (num_bytes_sent < 0) show_final_message("Error sending data");
  if (num_bytes_sent == 0) show_final_message("Wrong number of bytes sent");

  close(socket_communication);
}