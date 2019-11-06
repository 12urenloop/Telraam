package telraam.beacon;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.List;
import java.util.ArrayList;

import java.util.logging.Level;
import java.util.logging.Logger;

public class TCPFactory<B> implements Event.EventHandler<B>, Runnable {
    private static Logger logger = Logger.getLogger(TCPFactory.class.getName());

    protected Callback<Void, Socket> creator;
    private ServerSocket socket;
    List<Callback<Void, B>> handlers = new ArrayList<>();
    List<Callback<Void, Exception>> errorHandlers = new ArrayList<>();
    List<Callback<Void, Void>> exitHandlers = new ArrayList<>();
    List<Callback<Void, Void>> connectHandlers = new ArrayList<>();

    public TCPFactory(Callback<Void, Socket> creator, int port) throws IOException {
        this(port);
        this.creator = creator;
    }

    protected TCPFactory(int port) throws IOException {
        if (port > 0)
            this.socket = new ServerSocket(port);
        logger.log(Level.INFO, "Starting tcp on port "+port);
    }

    public void run() {
        logger.log(Level.INFO, "Actually accepting connections");
        while (true) {
            try {
                Socket s = socket.accept();
                this.creator.handle(s);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }

    public TCPFactory<B> onError(Callback<Void, Exception> handler) {
        this.errorHandlers.add(handler);
        return this;
    }

    public TCPFactory<B> onData(Callback<Void, B> handler) {
        this.handlers.add(handler);
        return this;
    }

    public TCPFactory<B> onDisconnect(Callback<Void, Void> handler) {
        this.exitHandlers.add(handler);
        return this;
    }

    public TCPFactory<B> onConnect(Callback<Void, Void> handler) {
        this.connectHandlers.add(handler);
        return this;
    }

    public void exit() {
        this.exitHandlers.forEach((eh) -> eh.handle(null));
    }

    public void connect() {
        this.connectHandlers.forEach((th) -> th.handle(null));
    }

    public void error(Exception e) {
        this.errorHandlers.forEach((eh) -> eh.handle(e));
    }

    public void data(B t) {
        this.handlers.forEach((th) -> th.handle(t));
    }
}
