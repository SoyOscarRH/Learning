import java.net.*;
import java.io.*;
import java.util.*;

public class Server {
	public static final int port = 8000;
	final ServerSocket serverSocket;

	class Manager extends Thread {
		private Socket socket;
		private BufferedOutputStream outputStream;

		public Manager(final Socket socket) {
			this.socket = socket;
		}

		public void run() {
			try {
				final var reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
				outputStream = new BufferedOutputStream(socket.getOutputStream());
				final var writer = new PrintWriter(new OutputStreamWriter(outputStream));

				try {
					final var line = reader.readLine();
					if (line == null) {
						writer.print("<!DOCTYPE html><html><head><meta charset=\"utf-8\" /><title>Servidor WEB");
						writer.print("</title><body bgcolor=\"#AACCFF\"<br>Linea Vacia</br>");
						writer.print("</body></html>");
						socket.close();
						return;
					}

					System.out.println("Client from: " + socket.getInetAddress());
					System.out.println("By port: " + socket.getPort());
					System.out.println("Data: " + line + "\n");

					if (line.toUpperCase().startsWith("HEAD")) {
						sendFile("index.html", true);
					} else if (line.toUpperCase().startsWith("GET")) {
						if (line.indexOf("?") != -1) {
							final var tokens = new StringTokenizer(line, "?");
							final var tokenType = tokens.nextToken();
							final var request = tokens.nextToken();
							System.out.println("Token1: " + tokenType);
							System.out.println("Token2: " + request);
							writer.println("HTTP/1.0 200 Okay");
							writer.println();
							writer.print("<html><head><title>Server web</title></head>");
							writer.print("<body><h1>Parameters...</h1>");
							writer.print("<h3><b>" + request + "</b></h3>");
							writer.print("</body></html>");
							writer.flush();
						} else {
							final var filename = getFileName(line);
							sendFile(filename.equals("") ? "index.html" : filename, false);
						}
					} else if (line.toUpperCase().startsWith("POST")) {
						writer.println("HTTP/1.0 201 Created");
						for (var i = 0; i < 6; ++i)
							System.out.println(reader.readLine());

						var items = reader.readLine();
						items = items.substring(items.trim().indexOf(" ") + 1);
						final var num = Integer.parseInt(items.trim()) + 2;
						reader.readLine();

						var l = "";
						for (var i = 0; i < num; ++i) {
							l += (char) reader.read();
						}

						final var pairs = l.split("&");
						for (var x : pairs) {
							final var a = x.split("=");
							System.out.println(a[0] + " is " + a[1]);
						}

						writer.println();
						writer.flush();
					} else {
						writer.println("HTTP/1.0 501 Not Implemented");
						writer.println();
					}
				} catch (final Exception e) {
					e.printStackTrace();
					writer.println("HTTP/1.0 500 Internal Server Error");
					writer.println();
				}

				writer.flush();
				outputStream.flush();
				socket.close();

			} catch (Exception e) {
				e.printStackTrace();
			}
		}

		String getFileName(final String line) {
			if (line.toUpperCase().startsWith("GET")) {
				final int start = line.indexOf("/");
				final int end = line.indexOf(" ", start);
				return line.substring(start + 1, end);
			}

			return null;
		}

		public void sendFile(final String filename, final boolean justHeader) {
			try {
				int bytesRead = 0;
				final BufferedInputStream fileReader = new BufferedInputStream(new FileInputStream(filename));
				final var buffer = new byte[1024];

				if (fileReader.available() < 1024) {
					fileReader.available();
				}

				var header = "";
				header += "HTTP/1.0 200 ok\n";
				header += "Server: Oscar Server /1.0\n";
				header += "Date: " + new Date() + "\n";
				header += "Content-Type: text/html\n";
				header += "Content-Length: " + fileReader.available() + " \n";
				header += "\n";

				outputStream.write(header.getBytes());
				outputStream.flush();

				if (!justHeader) {
					while ((bytesRead = fileReader.read(buffer, 0, buffer.length)) != -1) {
						outputStream.write(buffer, 0, bytesRead);
					}
				}

				outputStream.flush();
				fileReader.close();
			} catch (final Exception e) {
				e.printStackTrace();
			}
		}

	}

	public Server() throws Exception {
		System.out.println("Server is starting...");
		this.serverSocket = new ServerSocket(port);

		System.out.println("Waiting for client :D\n");
		while (true) {
			new Manager(serverSocket.accept()).start();
		}
	}

	public static void main(final String[] filenames) throws Exception {
		new Server();
	}

}