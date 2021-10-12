package telraam.beacon;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The meat and potato's, but actually just spawning new connections with a
 * creator.
 *
 * @author Arthur Vercruysse
 */
public class TCPFactory<B> implements Runnable {
    private static Logger logger = Logger.getLogger(TCPFactory.class.getName());
    protected Callback<Void, Socket> creator;
    private boolean stop = false;
    private ServerSocket socket;

    public TCPFactory(Callback<Void, Socket> creator, int port)
            throws IOException {
        this(port);
        this.creator = creator;
    }

    protected TCPFactory(int port) throws IOException {
        if (port > 0)
            this.socket = new ServerSocket(port);
        logger.log(Level.INFO, "Starting tcp on port {}", port);
    }

    protected TCPFactory() throws IOException {
        this.socket = new ServerSocket();
        logger.log(Level.INFO, "Starting tcp on port {}", this.socket.getLocalPort());
    }

    public void run() {
        logger.log(Level.INFO, "Actually accepting connections");
        try {
            this.runLoop();
        } catch (IOException e) {
            e.printStackTrace();
            throw new TCPFactoryException(e);
        }
    }

    private void runLoop() throws IOException {
        while (!this.stop) {
            Socket s = socket.accept();
            this.creator.handle(s);
        }
        socket.close();

    }

    public void kill() {
        this.stop = true;
    }

    private static class TCPFactoryException extends RuntimeException {
        TCPFactoryException(IOException error) {
            super(String.format("TCP runloop crashed %s", error.getMessage()));
        }
    }
}
