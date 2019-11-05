package telraam.beacon;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.List;
import java.util.ArrayList;

public class TCPFactory<B> implements Event.EventHandler<B>, Runnable {
    protected Callback<Void, Socket> creator;
    private ServerSocket socket;
    List<Callback<Void, B>> handlers = new ArrayList<>();
    List<Callback<Void, Exception>> errorHandlers = new ArrayList<>();
    List<Callback<Void, Void>> exitHandlers = new ArrayList<>();

    public TCPFactory(Callback<Void, Socket> creator, int port) throws IOException {
        this(port);
        this.creator = creator;

        new Thread(this).run();
    }

    protected TCPFactory(int port) throws IOException {
        this.socket = new ServerSocket(port);
    }

    public void run() {
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

    public void exit() {
        this.exitHandlers.forEach((eh) -> eh.handle(null));
    }

    public void error(Exception e) {
        // Hiding types with lambda's
        this.errorHandlers.forEach((eh) -> eh.handle(e));
    }

    public void data(B t) {
        this.handlers.forEach((th) -> th.handle(t));
    }
}
