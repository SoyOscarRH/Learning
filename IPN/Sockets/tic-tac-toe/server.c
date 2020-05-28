#include <arpa/inet.h>
#include <netinet/in.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#define EXAMPLE_GROUP "239.0.0.1"

void error(const char* mensaje);
void enviarMensaje(int cli_sockfd, char* mensaje);
void enviarEntero(int cli_sockfd, int mensaje);
void enviarMensajes(int* cli_sockfd, char* mensaje);
void enviarEnteros(int* cli_sockfd, int mensaje);
void obtenerClientes(int lis_sockfd, int* cli_sockfd);
void actualizarTablero(char tablero[][3], int movimiento, int idJugador);
void enviarTableroActualizado(int* cli_sockfd, int movimiento, int idJugador);
void enviarNumeroJugadores(int cli_sockfd);

void* correrJuego(void* thread_data);
void* hiloEnviarPuerto();
void* hiloIniciarJuego();

int recibirEntero(int cli_sockfd);
int setup_listener(int puertoNumero);
int obtenerMovimiento(int cli_sockfd);
int checarMovimiento(char tablero[][3], int movimiento, int idJugador);
int checarTablero(char tablero[][3], int ultimoMovimiento);

int numeroJugadores = 0;
int puertoGlobal;
int totalConexiones = 0;

pthread_mutex_t mutexcount;

int main(int argc, char* argv[]) {
  if (argc < 2) {
    fprintf(stderr, "ERROR: No se especifico puerto\n");
    exit(1);
  }

  puertoGlobal = atoi(argv[1]);
  pthread_t hiloMulti, hiloJuego;

  pthread_create(&hiloMulti, NULL, hiloEnviarPuerto,
                 NULL);  // Hilo del envio del puerto por multicast
  pthread_create(&hiloJuego, NULL, hiloIniciarJuego, NULL);  // Hilo de la partida del juego
  pthread_join(hiloMulti, NULL);
  pthread_join(hiloJuego, NULL);
}

/**
 * Método que imprime un mensaje. de error
 *
 * @param mensaje mensaje a imprimir
 */
void error(const char* mensaje) {
  perror(mensaje);
  pthread_exit(NULL);
}

/**
 * Método que recibe una variable de tipo entero del cliente
 *
 * @param click_sockfd entero a recibir
 * @return mensaje en forma de entero
 */
int recibirEntero(int cli_sockfd) {
  int mensaje = 0;
  int n = read(cli_sockfd, &mensaje, sizeof(int));

  if (n < 0 || n != sizeof(int)) /* Cliente desconectado. */
    return -1;

#ifdef DEBUG
  printf("[DEBUG] Entero recibido: %d\n", mensaje);
#endif

  return mensaje;
}

/*
 * Funciones de escritura de socket
 */

/**
 * Método que envia un mensaje al cliente
 *
 * @param cli_sockfd cliente al que se le envia el mensaje
 * @param mensaje mensaje a enviar
 */
void enviarMensaje(int cli_sockfd, char* mensaje) {
  int n = write(cli_sockfd, mensaje, strlen(mensaje));
  if (n < 0) error("ERROR escribiendo mensaje a socket cliente");
}

/**
 * Método que envia un entero al cliente
 *
 * @param cli_sockfd cliente al que se le envia el mensaje
 * @param mensaje entero a enviar
 */
void enviarEntero(int cli_sockfd, int mensaje) {
  int n = write(cli_sockfd, &mensaje, sizeof(int));
  if (n < 0) error("ERROR escribiendo entero a socket cliente");
}

/**
 * Método que envia un mensaje a dos clientes
 *
 * @param cli_sockfd clientes a los que se les envia el mensaje
 * @param mensaje mensaje a enviar
 */
void enviarMensajes(int* cli_sockfd, char* mensaje) {
  enviarMensaje(cli_sockfd[0], mensaje);
  enviarMensaje(cli_sockfd[1], mensaje);
}

/**
 * Método que envia un entero a dos clientes
 *
 * @param cli_sockfd cliente al que se le envia el mensaje
 * @param mensaje entero a enviar
 */
void enviarEnteros(int* cli_sockfd, int mensaje) {
  enviarEntero(cli_sockfd[0], mensaje);
  enviarEntero(cli_sockfd[1], mensaje);
}

/*
 * Funciones de conexion
 */

/* Configura listener socket. */
int setup_listener(int portno) {
  int sockfd;
  struct sockaddr_in serv_addr;

  /* Get a socket to listen on */
  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) error("ERROR al abrir socket cliente.");

  /* Zero out the memory for the server information */
  memset(&serv_addr, 0, sizeof(serv_addr));

  /* set up the server info */
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = INADDR_ANY;
  serv_addr.sin_port = htons(portno);

  /* Bind the server info to the listener socket. */
  if (bind(sockfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr)) < 0)
    error("ERROR binding listener socket.");

#ifdef DEBUG
  printf("[DEBUG] Listener set.\n");
#endif

  /* Return the socket number. */
  return sockfd;
}

/**
 * Método que configura las conexiones entre clientes
 *
 * @param lis_sockfd
 * @param cli_sockfd clientes
 */
void obtenerClientes(int lis_sockfd, int* cli_sockfd) {
  socklen_t clilen;
  struct sockaddr_in serv_addr, cli_addr;

#ifdef DEBUG
  printf("[DEBUG] Escucha por clientes...\n");
#endif

  /* Escucha por dos clientes. */
  int numeroConexiones = 0;
  while (numeroConexiones < 2 && totalConexiones < 4) {
    /* Escucha por clientes. */
    listen(lis_sockfd, 253 - numeroJugadores);

    /* Zero out memory for the client information. */
    memset(&cli_addr, 0, sizeof(cli_addr));

    clilen = sizeof(cli_addr);

    /*Acepta la conexion desde el cliente. */
    cli_sockfd[numeroConexiones] = accept(lis_sockfd, (struct sockaddr*)&cli_addr, &clilen);
    totalConexiones++;
    if (cli_sockfd[numeroConexiones] < 0) /* Algo no ha salido bien. */
      error("ERROR al aceptar conexion del cliente.");

#ifdef DEBUG
    printf("[DEBUG] Conexion aceptada del cliente %d\n", numeroConexiones);
#endif

    /* Send the client it's ID. */
    write(cli_sockfd[numeroConexiones], &numeroConexiones, sizeof(int));

#ifdef DEBUG
    printf("[DEBUG]Envia clinte %d su ID.\n", numeroConexiones);
#endif

    /* Incrementa el contador del jugador. */
    pthread_mutex_lock(&mutexcount);
    numeroJugadores++;
    printf("El numero de jugadores es ahora %d.\n", numeroJugadores);
    pthread_mutex_unlock(&mutexcount);

    if (numeroConexiones == 0) {
      /* Envia "HLD" al primer cliente para que sepa el usuario que el servidor esta esperando un
       * segundo jugador. */
      enviarMensaje(cli_sockfd[0], "HLD");

#ifdef DEBUG
      printf("[DEBUG] Told client 0 to hold.\n");
#endif
    }

    numeroConexiones++;
  }
}

/*
 * Game Functions
 */

/**
 * Método que recibe un movimiento del jugador
 *
 * @param cli_sockfd cliente del que se obtiene el movimiento
 * @return movimiento del jugador
 */
int obtenerMovimiento(int cli_sockfd) {
#ifdef DEBUG
  printf("[DEBUG] Obteniendo movimiento del jugador...\n");
#endif

  /* Decirle al jugador que haga un movimiento. */
  enviarMensaje(cli_sockfd, "TRN");

  /* Recibe el movimiento de los jugadores. */
  return recibirEntero(cli_sockfd);
}

/**
 * Método que checa la validez del movimiento de un jugador
 *
 * @param tablero tablero del juego
 * @param movimiento movimiento del jugador
 * @param idJugador jugador que hizo el movimiento
 * @return validez del movimiento
 */
int checarMovimiento(char tablero[][3], int movimiento, int idJugador) {
  if ((movimiento == 9) ||
      (tablero[movimiento / 3][movimiento % 3] == ' ')) { /* Movimiento valido. */

#ifdef DEBUG
    printf("[DEBUG] Movimiento valido del Jugador %d'.\n", idJugador);
#endif

    return 1;
  } else { /* Movimiento invalido. */
#ifdef DEBUG
    printf("[DEBUG] Movimiento invalido del Jugador %d.\n", idJugador);
#endif

    return 0;
  }
}

/**
 * Método que actualiza el tablero del juego
 *
 * @param tablero tablero del juego
 * @param movimiento movimiento del jugador
 * @param idJugador jugador que hizo el movimiento
 */
void actualizarTablero(char tablero[][3], int movimiento, int idJugador) {
  tablero[movimiento / 3][movimiento % 3] = idJugador ? 'X' : 'O';

#ifdef DEBUG
  printf("[DEBUG] Tablero actualizado.\n");
#endif
}

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
 * Método que actualiza el tablero del juego en ambos clientes
 *
 * @param cli_sockfd jugador
 * @param movimiento movimiento del jugador
 * @param idJugador jugador
 */
void enviarTableroActualizado(int* cli_sockfd, int movimiento, int idJugador) {
#ifdef DEBUG
  printf("[DEBUG] Envia actualizacion...\n");
#endif

  /* Señal de actualizacion */
  enviarMensajes(cli_sockfd, "UPD");

  /* Envia el id del jugador que hizo el movimiento. */
  enviarEnteros(cli_sockfd, idJugador);

  /* Envia el movimiento */
  enviarEnteros(cli_sockfd, movimiento);

#ifdef DEBUG
  printf("[DEBUG] Actualizacion enviada.\n");
#endif
}

/**
 * Método que envia el numero de jugadores a un jugador
 *
 * @param cli_sockfd jugador
 */
void enviarNumeroJugadores(int cli_sockfd) {
  enviarMensaje(cli_sockfd, "CNT");
  enviarEntero(cli_sockfd, numeroJugadores);

#ifdef DEBUG
  printf("[DEBUG] Envio del contador al jugador.\n");
#endif
}

/**
 * Método que checa con base en el tablero actual si hay ganador o no
 *
 * @param tablero tablero del juego
 * @param ultimoMovimiento ultimo movimiento realizado en el juego
 * @return si existe un ganador aun
 */
int checarTablero(char tablero[][3], int ultimoMovimiento) {
#ifdef DEBUG
  printf("[DEBUG] Checa por un ganador...\n");
#endif

  int fila = ultimoMovimiento / 3;
  int columna = ultimoMovimiento % 3;

  if (tablero[fila][0] == tablero[fila][1] &&
      tablero[fila][1] == tablero[fila][2]) { /* Checa fila. */
#ifdef DEBUG
    printf("[DEBUG] Gana por fila %d.\n", fila);
#endif
    return 1;
  } else if (tablero[0][columna] == tablero[1][columna] &&
             tablero[1][columna] == tablero[2][columna]) { /* Checa columna. */
#ifdef DEBUG
    printf("[DEBUG] Gano por columna %d.\n", columna);
#endif
    return 1;
  } else if (!(ultimoMovimiento % 2)) { /* Checa las diagonales */
    if ((ultimoMovimiento == 0 || ultimoMovimiento == 4 || ultimoMovimiento == 8) &&
        (tablero[1][1] == tablero[0][0] &&
         tablero[1][1] == tablero[2][2])) { /* Checa diagonal invertida. */
#ifdef DEBUG
      printf("[DEBUG] Gana por diagonal invertida.\n");
#endif
      return 1;
    }
    if ((ultimoMovimiento == 2 || ultimoMovimiento == 4 || ultimoMovimiento == 6) &&
        (tablero[1][1] == tablero[0][2] && tablero[1][1] == tablero[2][0])) { /* Checa diagonal. */
#ifdef DEBUG
      printf("[DEBUG] Gana por diagonal.\n");
#endif
      return 1;
    }
  }

#ifdef DEBUG
  printf("[DEBUG] No hay ganador todavia.\n");
#endif

  /* Nadie gana aun. */
  return 0;
}

/**
 * Hilo que corre un juego
 *
 * @param thread_data datos del hilo
 */
void* correrJuego(void* thread_data) {
  int* cli_sockfd = (int*)thread_data;   /* Sockets cliente. */
  char tablero[3][3] = {{' ', ' ', ' '}, /* Tablero */
                        {' ', ' ', ' '},
                        {' ', ' ', ' '}};

  printf("Game on!\n");

  /* Mensaje de entrada. */
  enviarMensajes(cli_sockfd, "SRT");

#ifdef DEBUG
  printf("[DEBUG] Envio de mensaje que ha comenzado.\n");
#endif

  dibujarTablero(tablero);

  int turnoPrevioJugador = 1;
  int turnoJugador = 0;
  int juegoTerminado = 0;
  int contadorTurno = 0;
  while (!juegoTerminado) {
    /* Dile al otro jugador que espere */
    if (turnoPrevioJugador != turnoJugador)
      enviarMensaje(cli_sockfd[(turnoJugador + 1) % 2], "WAT");

    int valid = 0;
    int movimiento = 0;
    while (!valid) { /*Checar hasta que el movimiento del jugador sea valido. */
      movimiento = obtenerMovimiento(cli_sockfd[turnoJugador]);
      if (movimiento == -1) break; /* Error leer cliente socket. */

      printf("<<Jugador %d jugo la posicion %d>>\n", turnoJugador, movimiento);

      valid = checarMovimiento(tablero, movimiento, turnoJugador);
      if (!valid) { /* Movimiento invalido. */
        printf("<<Movimiento no valido. Intentalo otra vez>>\n");
        enviarMensaje(cli_sockfd[turnoJugador], "INV");
      }
    }

    if (movimiento == -1) { /* Error reading from client. */
      printf(">>Jugador desconectado<<\n");
      break;
    } else if (movimiento == 9) { /* Envia al cliente el numero de jugadores. */
      turnoPrevioJugador = turnoJugador;
      enviarNumeroJugadores(cli_sockfd[turnoJugador]);
    } else {
      /* Actualiza el tablero y notifica. */
      actualizarTablero(tablero, movimiento, turnoJugador);
      enviarTableroActualizado(cli_sockfd, movimiento, turnoJugador);

      /* Redibuja tablero. */
      dibujarTablero(tablero);

      /* Checa si hay ganador/perdedor. */
      juegoTerminado = checarTablero(tablero, movimiento);

      if (juegoTerminado == 1) { /* Ganador. */
        enviarMensaje(cli_sockfd[turnoJugador], "WIN");
        enviarMensaje(cli_sockfd[(turnoJugador + 1) % 2], "LSE");
        printf(">>Jugador %d gana,\n", turnoJugador);
      } else if (contadorTurno == 8) { /* Empate. */
        printf(">>Pinta.\n");
        enviarMensajes(cli_sockfd, "DRW");
        juegoTerminado = 1;
      }

      /* Siguiente jugador. */
      turnoPrevioJugador = turnoJugador;
      turnoJugador = (turnoJugador + 1) % 2;
      contadorTurno++;
    }
  }

  printf("<<Juego terminado>>\n");

  /* Cierra los sockets cliente y decremente el contador de no. de jugadores. */
  close(cli_sockfd[0]);
  close(cli_sockfd[1]);

  pthread_mutex_lock(&mutexcount);
  numeroJugadores--;
  totalConexiones--;
  printf("<<Numero de jugadores es %d>>", numeroJugadores);
  numeroJugadores--;
  totalConexiones--;
  printf("<<Numero de jugadores es %d>>", numeroJugadores);
  pthread_mutex_unlock(&mutexcount);

  free(cli_sockfd);

  pthread_exit(NULL);
}

/**
 * Hilo que envia mediante datagramas el numero de puerto al que se está conectando
 */
void* hiloEnviarPuerto() {
  struct sockaddr_in addr;
  int addrlen, sock, cnt;
  char message[50];

  /* set up socket */
  sock = socket(AF_INET, SOCK_DGRAM, 0);
  if (sock < 0) {
    perror("socket");
    exit(1);
  }
  bzero((char*)&addr, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  addr.sin_port = htons(9000);
  addrlen = sizeof(addr);

  // Enviar
  addr.sin_addr.s_addr = inet_addr(EXAMPLE_GROUP);
  while (1) {
    time_t t = time(0);
    sprintf(message, "%d", puertoGlobal);
    printf("Enviando: %s\n", message);
    if (totalConexiones < 4) {
      cnt = sendto(sock, message, sizeof(message), 0, (struct sockaddr*)&addr, addrlen);
      if (cnt < 0) {
        perror("sendto");
        exit(1);
      }
    }
    sleep(5);
  }
}

/**
 * Hilo que inicia la partida entre dos jugadores
 */
void* hiloIniciarJuego() {
  int lis_sockfd = setup_listener(puertoGlobal);  // Listener socket.

  pthread_mutex_init(&mutexcount, NULL);

  printf(">>Iniciar juego");

  while (1) {
    if (numeroJugadores <= 2) {                         // Inicia el juego si hay espacio.
      int* cli_sockfd = (int*)malloc(2 * sizeof(int));  // Cliente sockets
      memset(cli_sockfd, 0, 2 * sizeof(int));

      // Obtiene dos clientes conectados
      obtenerClientes(lis_sockfd, cli_sockfd);

#ifdef DEBUG
      printf("[DEBUG] <<Iniciando nuevo hilo de juego>>\n");
#endif

      pthread_t thread;
      int result = pthread_create(&thread, NULL, correrJuego,
                                  (void*)cli_sockfd);  // Empieza un nuevo hilo para el juego
      if (result) {
        printf("Fallo en la creacion de hilo con codigo %d\n", result);
        exit(-1);
      }

#ifdef DEBUG
      printf("[DEBUG] <<Nuevo hilo de juego iniciado>>\n");
#endif
    }
  }

  close(lis_sockfd);

  pthread_mutex_destroy(&mutexcount);
  pthread_exit(NULL);
}