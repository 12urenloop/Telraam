package telraam.websocket;

import com.fasterxml.jackson.core.JsonGenerationException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

@NoArgsConstructor
public class WebSocketMessageSingleton {
    private static final Logger logger = Logger.getLogger(WebSocketMessageSingleton.class.getName());

    @Getter
    private static final WebSocketMessageSingleton instance = new WebSocketMessageSingleton();
    private static final Set<WebSocketConnection> registeredConnections = new HashSet<>();
    private final ObjectMapper mapper = new ObjectMapper();

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

    public void sendToAll(WebSocketMessage<?> message) {
        try {
            String json = mapper.writeValueAsString(message);
            this.sendToAll(json);
        } catch (JsonProcessingException e) {
            logger.severe("Json conversion error for \"%s\"".formatted(message.toString()));
        }
    }
}
