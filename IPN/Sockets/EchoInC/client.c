#include <arpa/inet.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "error.h"

const size_t buffer_size = 2000;

int main(int argc, char** argv) {
  if (argc < 3 || argc > 4) show_final_message("Use: <server address> <word> [<port>]");

  char* const server_address = argv[1];
  char* const word_to_send = argv[2];

  in_port_t port = (argc == 4) ? atoi(argv[3]) : 7;

  const int socket_id = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (socket_id < 0) show_final_message("Error opening a socket");

  struct sockaddr_in server;
  {
    memset(&server, 0, sizeof(server));
    server.sin_family = AF_INET;
    const int error_code = inet_pton(AF_INET, server_address, &server.sin_addr.s_addr);
    if (error_code == 0) show_final_message("wrong server address");
    if (error_code < 0) show_final_message("Error in inet_pton()");
    server.sin_port = htons(port);
  }

  // We establish communication with the server
  const int error = connect(socket_id, (struct sockaddr*)&server, sizeof(server));
  if (error < 0) show_final_message("Error in establishing the connection");

  const size_t bytes_to_send = strlen(word_to_send);

  // Send to server
  const ssize_t num_bytes = send(socket_id, word_to_send, bytes_to_send, 0);
  if (num_bytes < 0)
    show_final_message("Error sending data");
  else if (num_bytes != bytes_to_send)
    show_final_message("Wrong number of bytes sent");

  // Getting data back
  int bytes_received = 0;
  while (bytes_received < bytes_to_send) {
    char buffer[buffer_size] = {0};
    const int num_bytes = recv(socket_id, buffer, buffer_size, 0);

    if (num_bytes < 0) show_final_message("Failed reception");
    if (num_bytes == 0) show_final_message("Connection closed prematurely");
    bytes_received += num_bytes;
    printf("Received: %s\n", buffer);
  }

  close(socket_id);
  return 0;
}
