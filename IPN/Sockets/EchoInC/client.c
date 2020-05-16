#include <arpa/inet.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#include "error.h"

const size_t buffer_size = 2000;

int setup(const char* server_address, const in_port_t port);
void send_data(const int socket_fd, const void* data, const size_t how_much);
void get_data(const int socket_fd, void* data, const size_t how_much);

int main(int argc, char** argv) {
  if (argc != 7)
    show_final_message("Use: <server address> <port> <[int]> <[float]> <[string]> <[double]>");

  const char* server_address = argv[1];
  const in_port_t port = atoi(argv[2]);
  const int socket_fd = setup(server_address, port);

  // int
  const int number_to_send = atoi(argv[3]);
  send_data(socket_fd, &number_to_send, sizeof(number_to_send));
  int number_to_get;
  get_data(socket_fd, &number_to_get, sizeof(number_to_send));
  printf("Received: %i\n", number_to_get);

  // float str
  char* float_str = argv[4];
  const size_t len_float = strlen(float_str) + 1;
  send_data(socket_fd, float_str, len_float);
  get_data(socket_fd, float_str, len_float);
  const float float_to_get = atof(float_str);
  printf("Received: %f\n", float_to_get);

  // string
  char* string_buffer = argv[5];
  const size_t len_str = strlen(string_buffer) + 1;
  send_data(socket_fd, string_buffer, len_str);
  get_data(socket_fd, string_buffer, len_str);
  printf("Received: %s\n", string_buffer);

  // double
  const double double_to_send = atof(argv[6]);
  send_data(socket_fd, &double_to_send, sizeof(double_to_send));
  double double_to_get;
  get_data(socket_fd, &double_to_get, sizeof(double_to_get));
  printf("Received: %lf\n", double_to_get);

  close(socket_fd);
  return 0;
}

int setup(const char* server_address, const in_port_t port) {
  const int socket_fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (socket_fd < 0) show_final_message("Error opening a socket");

  struct sockaddr_in server;
  memset(&server, 0, sizeof(server));
  server.sin_family = AF_INET;
  server.sin_port = htons(port);

  const int error_code = inet_pton(AF_INET, server_address, &server.sin_addr.s_addr);
  if (error_code == 0) show_final_message("wrong server address");
  if (error_code < 0) show_final_message("Error in inet_pton()");

  // We establish communication with the server
  const int error = connect(socket_fd, (struct sockaddr*)&server, sizeof(server));
  if (error < 0) show_final_message("Error in establishing the connection");

  return socket_fd;
}

void get_data(const int socket_fd, void* data, const size_t how_much) {
  const ssize_t bytes_received = recv(socket_fd, data, how_much, 0);
  if (bytes_received < 0) show_final_message("Failed reception");
  if (bytes_received == 0) show_final_message("Connection closed prematurely");
}

void send_data(const int socket_fd, const void* data, const size_t how_much) {
  const ssize_t bytes_sent = send(socket_fd, data, how_much, 0);
  if (bytes_sent < 0) show_final_message("Error sending data");
  if (bytes_sent != (ssize_t)how_much) show_final_message("Wrong number of bytes sent");
}
