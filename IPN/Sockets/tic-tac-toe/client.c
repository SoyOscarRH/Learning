#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <unistd.h>

int recibirEntero(int sockfd);
int conectarServidor(char* hostname, int portno);

void error(const char* mensaje);
void recibirMensaje(int sockfd, char* mensaje);
void enviarEntero(int sockfd, int mensaje);
void dibujarTablero(char tablero[][3]);
void insertarPosicion(int sockfd);
void actualizarTablero(int sockfd, char tablero[][3]);

void* hiloRecibirPuerto();

struct ip_mreq mreq;

int puertoGlobal;
int conectado = 0;

int main(int argc, char* argv[]) {
  pthread_t hiloBuscarPuerto, hiloJuego;
  pthread_create(&hiloBuscarPuerto, NULL, hiloRecibirPuerto,
                 NULL);  // Hilo del envio del puerto por Multicast
  pthread_join(hiloBuscarPuerto, NULL);

  /* Conecta con el servidor. */
  int sockfd = conectarServidor("127.0.0.1", puertoGlobal);

  /* Recibe cliente ID. */
  int id = recibirEntero(sockfd);

#ifdef DEBUG
  printf("[DEBUG] Cliente ID: %d\n", id);
#endif

  char mensaje[4];
  char tablero[3][3] = {{' ', ' ', ' '}, /* Tablero*/
                        {' ', ' ', ' '},
                        {' ', ' ', ' '}};

  printf("<<Juego del Gato>>\n\n");

  /* Espera a que el juego comienza. */
  do {
    recibirMensaje(sockfd, mensaje);
    if (!strcmp(mensaje, "HLD")) printf(">>Esperando al segundo jugador>>\n");
  } while (strcmp(mensaje, "SRT"));

  /* Ha comenzado el juego... */
  printf("<<Juego iniciado>>\n");
  printf("<<Te tocan las %c's>>\n", id ? 'X' : 'O');

  dibujarTablero(tablero);

  while (1) {
    recibirMensaje(sockfd, mensaje);

    if (!strcmp(mensaje, "TRN")) { /* Toma un turno. */
      printf(">>Tu turno\n");
      insertarPosicion(sockfd);
    } else if (!strcmp(mensaje, "INV")) { /* Movimiento invalido.  */
      printf("<<Posicion ya jugada. Vuelve a intentar>>\n");
    } else if (!strcmp(mensaje, "CNT")) { /* Recibiendo el numero de jugadores activos.*/
      int numeroJugadores = recibirEntero(sockfd);
      printf("<<En este momento hay %d jugadores activos>>\n", numeroJugadores);
    } else if (!strcmp(mensaje, "UPD")) { /* Servidor esta enviando el tablero actualizado. */
      actualizarTablero(sockfd, tablero);
      dibujarTablero(tablero);
    } else if (!strcmp(mensaje, "WAT")) { /* Espera por otro jugador a que tome un turno. */
      printf("<<Esperando un movimiento del otro jugador>>\n");
    } else if (!strcmp(mensaje, "WIN")) { /* Ganador. */
      printf(">>Ganaste<<\n");
      break;
    } else if (!strcmp(mensaje, "LSE")) { /* Perdedor. */
      printf(">>Perdiste<<\n");
      break;
    } else if (!strcmp(mensaje, "DRW")) { /* Empate. */
      printf(">>Empate<<\n");
      break;
    } else /* Algo salio mal... */
      error(">>Mensaje desconocido<<");
  }

  printf("<<Juego terminado>>\n\n");

  /* Cierra servidor y cierra. */
  close(sockfd);
  return 0;
}

/**
 * Método que imprime un mensaje. de error
 *
 * @param mensaje mensaje a imprimir
 */
void error(const char* mensaje) {
#ifdef DEBUG
  perror(mensaje);
#else
  printf(">>Servidor offline o jugador desconectado.\n<<Juego terminado>>\n");
#endif

  exit(0);
}

/*
 * Funciones de lectura de socket
 */

/**
 * Método que recibe un mensaje de tipo entero del servidor
 *
 * @param sockfd entero a recibir
 * @param mensaje mensaje a recibir
 */
void recibirMensaje(int sockfd, char* mensaje) {
  /* Mensajes de 3 bytes. */
  memset(mensaje, 0, 4);
  int n = read(sockfd, mensaje, 3);

  if (n < 0 || n != 3) /* Servidor u otro cliente desconectado. */
    error("ERROR: No se pudo recibir un mensaje del servidor.");

#ifdef DEBUG
  printf("[DEBUG] Mensaje recibido: %s\n", mensaje);
#endif
}

/**
 * Método que recibe una variable de tipo entero del servidor
 *
 * @param click_sockfd entero a recibir
 * @return mensaje en forma de entero
 */
int recibirEntero(int sockfd) {
  int mensaje = 0;
  int n = read(sockfd, &mensaje, sizeof(int));

  if (n < 0 || n != sizeof(int)) error("ERROR: No se pudo recibir un int del servidor");

#ifdef DEBUG
  printf("[DEBUG] Entero recibido: %d\n", mensaje);
#endif

  return mensaje;
}

/*
 * Funciones de escritura
 */

/**
 * Método que envia un entero al servidor
 *
 * @param cli_sockfd cliente al que se le envia el mensaje
 * @param mensaje entero a enviar
 */
void enviarEntero(int sockfd, int mensaje) {
  int n = write(sockfd, &mensaje, sizeof(int));
  if (n < 0) error("ERROR: No se pudo enviar un entero al servidor");

#ifdef DEBUG
  printf("[DEBUG] Entero enviado al servidor %d\n", mensaje);
#endif
}

/*
 * Funciones de conexion
 */

/**
 * Método que configura la conexión al servidor
 *
 * @param portnocliente numero de puerto
 * @param hostname nombre de host
 */
int conectarServidor(char* hostname, int portno) {
  struct sockaddr_in serv_addr;
  struct hostent* server;

  /* Obtiene un socket. */
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);

  if (sockfd < 0) error("ERROR: No se pudo abrir socket de servidor");

  /* Obtienes direccion. */
  server = gethostbyname(hostname);

  if (server == NULL) {
    fprintf(stderr, "ERROR: No existe host\n");
    exit(0);
  }

  /* Zero out memory for server info. */
  memset(&serv_addr, 0, sizeof(serv_addr));

  /* Informacion del servidor. */
  serv_addr.sin_family = AF_INET;
  memmove(server->h_addr_list, &serv_addr.sin_addr.s_addr, server->h_length);
  serv_addr.sin_port = htons(portno);

  /* Haz la conexion */
  if (connect(sockfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr)) < 0)
    error("ERROR: No se pudo conectar al servidor");

#ifdef DEBUG
  printf("[DEBUG] Conectado al server.\n");
#endif

  return sockfd;
}

/*
 * Game Functions
 */

/**
 * Método que dibuja el tablero del juego
 *
 * @param tablero tablero del juego
 */
void dibujarTablero(char tablero[][3]) {
  printf("     |     |     \n");
  printf("  %c  |  %c  |  %c \n", tablero[0][0], tablero[0][1], tablero[0][2]);
  printf("_____|_____|_____\n");
  printf("     |     |     \n");
  printf("  %c  |  %c  |  %c \n", tablero[1][0], tablero[1][1], tablero[1][2]);
  printf("_____|_____|_____\n");
  printf("     |     |     \n");
  printf("  %c  |  %c  |  %c \n", tablero[2][0], tablero[2][1], tablero[2][2]);
  printf("     |     |     \n");
}

/**
 * Método que dibuja el tablero del juego
 *
 * @param sockfd socket
 */
void insertarPosicion(int sockfd) {
  char buffer[10];

  while (1) { /* Pregunta hasta que reciba. */
    printf(">>Ingresa un numero del 0 al 8 para hacer un movimiento: ");
    fgets(buffer, 10, stdin);
    int movimiento = buffer[0] - '0';
    if (movimiento <= 9 && movimiento >= 0) {
      printf("\n");
      /* Envia movimiento del jugador al server. */
      enviarEntero(sockfd, movimiento);
      break;
    } else
      printf("\n<<Movimiento no valido. Intentalo de nuevo>>\n");
  }
}

/**
 * Método que actualiza el tablero del juego
 *
 * @param tablero tablero del juego
 * @param movimiento movimiento del jugador
 * @param idJugador jugador que hizo el movimiento
 */
void actualizarTablero(int sockfd, char tablero[][3]) {
  /* Obtiene la actualizacion. */
  int idJugador = recibirEntero(sockfd);
  int movimiento = recibirEntero(sockfd);

  /* Actualiza. */
  tablero[movimiento / 3][movimiento % 3] = idJugador ? 'X' : 'O';
}

/**
 * Hilo que recibe un puerto a donde conectarse
 */
void* hiloRecibirPuerto() {
  /* Create a datagram socket on which to receive. */
  struct sockaddr_in localSock;
  struct ip_mreq group;
  int sd, cnt, addrlen;
  int datalen;
  char databuf[1024];
  sd = socket(AF_INET, SOCK_DGRAM, 0);

  if (sd < 0) {
    perror("Error abriendo socket de datagrama");
    exit(1);
  } else
    printf("Abriendo socket de datagrama: Bien.\n");
  /* Enable SO_REUSEADDR to allow multiple instances of this */
  /* application to receive copies of the multicast datagrams. */
  {
    int reuse = 1;
    if (setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, (char*)&reuse, sizeof(reuse)) < 0) {
      perror("Error configurando SO_REUSEADDR");
      close(sd);
      exit(1);
    } else

      printf("Configurando SO_REUSEADDR: Bien.\n");
  }
  /* Bind to the proper port number with the IP address */
  /* specified as INADDR_ANY. */
  memset((char*)&localSock, 0, sizeof(localSock));
  localSock.sin_family = AF_INET;
  localSock.sin_port = htons(9000);
  localSock.sin_addr.s_addr = INADDR_ANY;

  if (bind(sd, (struct sockaddr*)&localSock, sizeof(localSock))) {
    perror("Error uniendo socket de datagrama");
    close(sd);
    exit(1);
  }

  else
    printf("Uniendo socket de datagrama: Bien.\n");

  /* Join the multicast group 226.1.1.1 on the local 203.106.93.94 */
  /* interface. Note that this IP_ADD_MEMBERSHIP option must be */
  /* called for each local interface over which the multicast */
  /* datagrams are to be received. */
  group.imr_multiaddr.s_addr = inet_addr("239.0.0.1");
  group.imr_interface.s_addr = htonl(INADDR_ANY);
  if (setsockopt(sd, IPPROTO_IP, IP_ADD_MEMBERSHIP, (char*)&group, sizeof(group)) < 0) {
    perror("Error al anadirse al grupo de multicast");
    close(sd);
  } else
    printf("Anadiendo al grupo de multicast: Bien\n");
  /* Read from the socket. */
  char mensaje[50];
  while (1) {
    cnt = recvfrom(sd, mensaje, sizeof(mensaje), 0, (struct sockaddr*)&localSock, &addrlen);
    if (cnt < 0) {
      perror("recvfrom");
      exit(1);
    } else if (cnt == 0) {
      break;
    }
    printf("%s: Mensaje = \"%s\"\n", inet_ntoa(localSock.sin_addr), mensaje);
    puertoGlobal = atoi(mensaje);
    break;
  }
}