package telraam.websocket;

import lombok.Getter;

import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

public class WebSocketMessageSingleton {
    private static final Logger logger = Logger.getLogger(WebSocketMessageSingleton.class.getName());

    @Getter
    private static final WebSocketMessageSingleton instance = new WebSocketMessageSingleton();
    private static final Set<WebSocketConnection> registeredConnections = new HashSet<>();

    private WebSocketMessageSingleton() {
    }

    public void registerConnection(WebSocketConnection conn) {
        boolean modified = registeredConnections.add(conn);
        if (modified) {
            logger.info("Registered WebSocketConnection %s".formatted(conn));
        }
    }

    public void unregisterConnection(WebSocketConnection conn) {
        boolean modified = registeredConnections.remove(conn);
        if (modified) {
            logger.info("Unregistered WebSocketConnection %s".formatted(conn));
        }
    }

    public void sendToAll(String s) {
        logger.finest("Sending \"%s\" to all registered WebSocketConnection instances".formatted(s));
        registeredConnections.forEach(conn -> conn.send(s));
    }
}
