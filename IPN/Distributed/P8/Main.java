import java.io.*;
import java.net.*;
import java.util.*;
import com.google.gson.*;

class Main {
  static enum OPTIONS {
    NEW_USER, GET_USER, DELETE_USER, DELETE_ALL_USER, EXIT, NOT_VALID
  };

  private static OPTIONS getOptions() {
    final var in = new Scanner(System.in);

    System.out.print("a. Alta usuario\n" + "b. Consulta usuario\n" + "c. Borra usuario\n"
        + "d. Borra todos los usuarios\n" + "e. Salir\n" + "Opción: ");

    final var data = in.nextLine().charAt(0);

    return switch (data) {
      case 'a' -> OPTIONS.NEW_USER;
      case 'b' -> OPTIONS.GET_USER;
      case 'c' -> OPTIONS.DELETE_USER;
      case 'd' -> OPTIONS.DELETE_ALL_USER;
      case 'e' -> OPTIONS.EXIT;
      default -> OPTIONS.NOT_VALID;
    };
  }

  static class Usuario {
    String email, nombre, apellido_paterno, apellido_materno, fecha_nacimiento, telefono, genero, photo;

    Usuario(final String email, final String nombre, final String apellido_paterno, final String apellido_materno,
        final String fecha_nacimiento, final String telefono, final String genero) {
      this.email = email;
      this.nombre = nombre;
      this.apellido_paterno = apellido_paterno;
      this.apellido_materno = apellido_materno;
      this.fecha_nacimiento = fecha_nacimiento;
      this.telefono = telefono;
      this.genero = genero.equals("M") ? "M" : genero.equals("F") ? "F" : null;
      this.photo = null;
    }

    @Override
    public String toString() {
      return String.format("email: %s\n", this.email).concat(String.format("nombre: %s\n", this.nombre))
          .concat(String.format("apellido_paterno: %s\n", this.apellido_paterno))
          .concat(String.format("apellido_materno: %s\n", this.apellido_materno))
          .concat(String.format("fecha_nacimiento: %s\n", this.fecha_nacimiento))
          .concat(String.format("telefono: %s\n", this.telefono)).concat(String.format("genero: %s\n", this.genero));
    }
  }

  private static Map<String, String> getResponse(final String place, final String params) throws Exception {
    final var url = new URL("http://40.69.144.11:8080/Servicio/rest/ws/" + place);
    final var http = (HttpURLConnection) url.openConnection();
    http.setDoOutput(true);
    http.setRequestMethod("POST");
    http.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");

    final var os = http.getOutputStream();
    os.write(params.getBytes());
    os.flush();

    if (http.getResponseCode() == 400) {
      var message = http.getResponseMessage();

      if (message == null) {
        final var br = new BufferedReader(new InputStreamReader((http.getErrorStream())));
        var current = "";
        message = "";
        while ((current = br.readLine()) != null) {
          message += current;
        }
      }

      http.disconnect();
      return Map.of("code", "400", "message", message);

    } else if (http.getResponseCode() != HttpURLConnection.HTTP_OK) {
      final var message = http.getResponseMessage();
      System.out.println(message);
      throw new RuntimeException("HTTP Error: " + http.getResponseCode());
    } else {
      final var br = new BufferedReader(new InputStreamReader((http.getInputStream())));
      var current = "";
      var response = "";
      while ((current = br.readLine()) != null) {
        response += current;
      }

      http.disconnect();
      return Map.of("code", "200", "message", response);
    }
  }

  private static void doNothing() {
    System.out.println("==== not a valid option ====");
  }

  private static void newUser() throws Exception {
    System.out.println("==== adding a new user ====");

    final var in = new Scanner(System.in);
    System.out.print("email: ");
    final var email = in.nextLine();

    System.out.print("nombre de usuario: ");
    final var username = in.nextLine();

    System.out.print("apellido 1: ");
    final var surname1 = in.nextLine();

    System.out.print("apellido 2: ");
    final var surname2 = in.nextLine();

    System.out.print("fecha de nacimiento (YYYY-MM-DD): ");
    final var birthdate = in.nextLine();

    System.out.print("telefono: ");
    final var phone = in.nextLine();

    System.out.print("genero (M|F): ");
    final var genre = in.nextLine();

    final var user = new Usuario(email, username, surname1, surname2, birthdate, phone, genre);
    final var params = (String) ("usuario=" + URLEncoder.encode(new Gson().toJson(user), "utf-8"));

    final var response = getResponse("alta", params);
    if (response.get("code").equals("200")) {
      System.out.println("\nOK");
    } else {
      System.out.println("\n" + response.get("message"));
    }
  }

  private static void getUser() throws Exception {
    System.out.println("==== get user ====");

    final var in = new Scanner(System.in);
    System.out.print("email: ");
    final var email = in.nextLine();

    final var params = "email=" + URLEncoder.encode(email, "utf-8");
    final var response = getResponse("consulta", params);

    if (response.get("code").equals("200")) {
      final var gson = new GsonBuilder().create();
      final var user = (Usuario) gson.fromJson(response.get("message"), Usuario.class);
      System.out.println("\n" + user);
    } else {
      System.out.println("\n" + response.get("message"));
    }
  }

  private static void deleteUser() throws Exception {
    System.out.println("==== delete user ====");

    final var in = new Scanner(System.in);
    System.out.print("email: ");
    final var email = in.nextLine();

    final var params = "email=" + URLEncoder.encode(email, "utf-8");
    final var response = getResponse("borra", params);

    if (response.get("code").equals("200")) {
      System.out.println("\nOK");
    } else {
      System.out.println("\n" + response.get("message"));
    }
  }

  private static void deleteAllUser() throws Exception {
    System.out.println("==== delete all user ====");

    final var in = new Scanner(System.in);
    final var response = getResponse("borratodos", "");

    if (response.get("code").equals("200")) {
      System.out.println("\nOK");
    } else {
      System.out.println("\n" + response.get("message"));
    }
  }

  private static void exit() {
    System.out.println("==== exiting :D ====");
    System.exit(0);
  }

  public static void main(String[] args) {
    var option = OPTIONS.NOT_VALID;

    try {
      while ((option = getOptions()) != OPTIONS.EXIT) {
        System.out.print("\n");
        switch (option) {
          case NEW_USER -> newUser();
          case GET_USER -> getUser();
          case DELETE_USER -> deleteUser();
          case DELETE_ALL_USER -> deleteAllUser();
          case EXIT -> exit();
          case NOT_VALID -> doNothing();
        }
        ;
        System.out.print("\n\n\n");
      }
    } catch (final Exception e) {
      e.printStackTrace();
      System.exit(1);
    }
  }

}
