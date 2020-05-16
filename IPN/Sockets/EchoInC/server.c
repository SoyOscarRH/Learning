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
#define BUFFER_SIZE 1024

void manage(int);

int setup(const in_port_t port);
void send_data(const int socket_fd, const void* data, const size_t how_much);
void get_data(const int socket_fd, void* data, const size_t how_much, ssize_t* bytes_received);

int main(int argc, char** argv) {
  if (argc != 2) show_final_message("Use: port");
  in_port_t port = atoi(argv[1]);

  const int socket_fd = setup(port);

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
    close(socket_communication);
  }
}

void manage(const int socket_communication) {
  ssize_t bytes_received;

  int number_to_get;
  get_data(socket_communication, &number_to_get, sizeof(number_to_get), &bytes_received);
  printf("[%zi bytes] \t %i\n", bytes_received, number_to_get);
  send_data(socket_communication, &number_to_get, bytes_received);

  char float_string_to_get[BUFFER_SIZE];
  get_data(socket_communication, float_string_to_get, BUFFER_SIZE, &bytes_received);
  printf("[%zi bytes] \t %s\n", bytes_received, float_string_to_get);
  send_data(socket_communication, float_string_to_get, bytes_received);

  char string_to_get[BUFFER_SIZE];
  get_data(socket_communication, string_to_get, BUFFER_SIZE, &bytes_received);
  printf("[%zi bytes] \t %s\n", bytes_received, string_to_get);
  send_data(socket_communication, string_to_get, bytes_received);

  double double_to_get;
  get_data(socket_communication, &double_to_get, sizeof(double_to_get), &bytes_received);
  printf("[%zi bytes] \t %f\n", bytes_received, double_to_get);
  send_data(socket_communication, &double_to_get, bytes_received);

  printf("\n");
}

int setup(const in_port_t port) {
  const int socket_fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (socket_fd < 0) show_final_message("Error opening a socket_communication");

  struct sockaddr_in server;
  memset(&server, 0, sizeof(server));
  server.sin_family = AF_INET;
  server.sin_port = htons(port);
  server.sin_addr.s_addr = htons(INADDR_ANY);

  const struct sockaddr* generic_address = (struct sockaddr*)&server;
  if (bind(socket_fd, generic_address, sizeof(server)) < 0) show_final_message("Error at binding");
  if (listen(socket_fd, max_waiting_connections) < 0) show_final_message("Error at listening");

  return socket_fd;
}

void get_data(const int socket_fd, void* data, const size_t how_much, ssize_t* bytes_received) {
  *bytes_received = recv(socket_fd, data, how_much, 0);
  if (*bytes_received < 0) show_final_message("Failed reception");
  if (*bytes_received == 0) show_final_message("Connection closed prematurely");
}

void send_data(const int socket_fd, const void* data, const size_t how_much) {
  const ssize_t bytes_sent = send(socket_fd, data, how_much, 0);
  if (bytes_sent < 0) show_final_message("Error sending data");
  if (bytes_sent != (ssize_t)how_much) show_final_message("Wrong number of bytes sent");
}
