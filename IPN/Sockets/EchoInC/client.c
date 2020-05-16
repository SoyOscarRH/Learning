#include <arpa/inet.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#include "error.h"

const size_t buffer_size = 2000;

int main(int argc, char** argv) {
  if (argc < 3 || argc > 4) show_final_message("Use: <server address> <word> [<port>]");

  char* const server_address = argv[1];
  char* const word_to_send = argv[2];

  in_port_t port = (argc == 4) ? atoi(argv[3]) : 7;

  const int socket_fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (socket_fd < 0) show_final_message("Error opening a socket");

  struct sockaddr_in server;
  {
    memset(&server, 0, sizeof(server));
    server.sin_family = AF_INET;
    server.sin_port = htons(port);

    const int error_code = inet_pton(AF_INET, server_address, &server.sin_addr.s_addr);
    if (error_code == 0) show_final_message("wrong server address");
    if (error_code < 0) show_final_message("Error in inet_pton()");
  }

  // We establish communication with the server
  const int error = connect(socket_fd, (struct sockaddr*)&server, sizeof(server));
  if (error < 0) show_final_message("Error in establishing the connection");

  // Send to server
  const size_t bytes_to_send = strlen(word_to_send);
  const ssize_t bytes_sent = send(socket_fd, word_to_send, bytes_to_send, 0);
  if (bytes_sent < 0)
    show_final_message("Error sending data");
  else if (bytes_sent != bytes_to_send)
    show_final_message("Wrong number of bytes sent");

  // Getting data back
  char buffer[buffer_size] = {0};
  const ssize_t bytes_received = recv(socket_fd, buffer, buffer_size, 0);

  if (bytes_received < 0) show_final_message("Failed reception");
  if (bytes_received == 0) show_final_message("Connection closed prematurely");
  printf("Received: %s\n", buffer);

  close(socket_fd);
  return 0;
}
