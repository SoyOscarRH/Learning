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

const char* PORT = "9034";

// Get sockaddr, IPv4 or IPv6:
void* get_in_addr(struct sockaddr* sa) {
  if (sa->sa_family == AF_INET) { return &(((struct sockaddr_in*)sa)->sin_addr); }

  return &(((struct sockaddr_in6*)sa)->sin6_addr);
}

// Return a listening socket
int get_listener_socket() {
  int listener;  // Listening socket descriptor
  int rv;

  struct addrinfo hints, *ai, *p;

  // Get us a socket and bind it
  memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;
  if ((rv = getaddrinfo(NULL, PORT, &hints, &ai)) != 0) {
    fprintf(stderr, "selectserver: %s\n", gai_strerror(rv));
    exit(1);
  }

  for (p = ai; p != NULL; p = p->ai_next) {
    listener = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
    if (listener < 0) { continue; }

    // Lose the pesky "address already in use" error message
    int yes = 1;  // For setsockopt() SO_REUSEADDR, below
    setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));

    if (bind(listener, p->ai_addr, p->ai_addrlen) < 0) {
      close(listener);
      continue;
    }

    break;
  }

  // If we got here, it means we didn't get bound
  if (p == NULL) return -1;

  freeaddrinfo(ai);  // All done with this

  // Listen
  if (listen(listener, 10) == -1) { return -1; }

  return listener;
}

int main() {
  int listener;  // Listening socket descriptor

  struct sockaddr_storage remoteaddr;  // Client address
  socklen_t addrlen;

  char buf[256];  // Buffer for client data

  char remoteIP[INET6_ADDRSTRLEN];

  // Start off with room for 5 connections
  // (We'll realloc as necessary)
  auto to_monitor = std::vector<struct pollfd> {};

  // Set up and get a listening socket
  listener = get_listener_socket();

  if (listener == -1) {
    fprintf(stderr, "error getting listening socket\n");
    exit(1);
  }

  struct pollfd listener_fd;
  listener_fd.fd = listener;
  listener_fd.events = POLLIN;
  to_monitor.push_back(listener_fd);

  while (true) {
    int poll_count = poll(to_monitor.data(), to_monitor.size(), -1);

    if (poll_count == -1) {
      perror("poll error");
      exit(1);
    }

    for (int i = 0; i < (int)to_monitor.size(); i++) {
      // Check if someone's ready to read
      if (to_monitor[i].revents & POLLIN) {  // We got one!!
        if (to_monitor[i].fd == listener) {
          // If listener is ready to read, handle new connection

          addrlen = sizeof remoteaddr;
          auto newfd = accept(listener, (struct sockaddr*)&remoteaddr, &addrlen);

          if (newfd == -1) {
            perror("accept");
          } else {
            struct pollfd new_fd;
            new_fd.fd = newfd;
            new_fd.events = POLLIN;
            to_monitor.push_back(new_fd);

            printf(
                "pollserver: new connection from %s on "
                "socket %d\n",
                inet_ntop(remoteaddr.ss_family, get_in_addr((struct sockaddr*)&remoteaddr),
                          remoteIP, INET6_ADDRSTRLEN),
                newfd);
          }
        } else {
          // If not the listener, we're just a regular client
          int nbytes = recv(to_monitor[i].fd, buf, sizeof buf, 0);

          int sender_fd = to_monitor[i].fd;

          if (nbytes <= 0) {
            if (nbytes == 0) printf("pollserver: socket %d hung up\n", sender_fd);
            else
              perror("recv");

            close(to_monitor[i].fd);
            to_monitor.pop_back();
          } else {
            // We got some good data from a client
            for (auto& j : to_monitor) {
              // Send to everyone!
              int dest_fd = j.fd;
              // Except the listener and ourselves
              if (dest_fd != listener && dest_fd != sender_fd) {
                if (send(dest_fd, buf, nbytes, 0) == -1) { perror("send"); }
              }
            }
          }
        }
      }
    }
  }

  return 0;
}
