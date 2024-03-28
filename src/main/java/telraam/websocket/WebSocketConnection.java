package telraam.websocket;

import java.io.IOException;
import java.util.logging.Logger;

import org.eclipse.jetty.websocket.api.Session;
import org.eclipse.jetty.websocket.api.WebSocketAdapter;

public class WebSocketConnection extends WebSocketAdapter {
    private static final Logger logger = Logger.getLogger(WebSocketConnection.class.getName());

    @Override
    public void onWebSocketConnect(Session session) {
        super.onWebSocketConnect(session);
        WebSocketMessageSingleton.getInstance().registerConnection(this);
        logger.info("Instance with remote \"%s\" connected".formatted(getRemote().getRemoteAddress()));
    }

    @Override
    public void onWebSocketClose(int statusCode, String reason) {
        super.onWebSocketClose(statusCode, reason);
        WebSocketMessageSingleton.getInstance().unregisterConnection(this);
        logger.info("Instance with remote \"%s\" closed: [%s] %s".formatted(getRemote().getRemoteAddress(), statusCode, reason));
    }

    @Override
    public void onWebSocketError(Throwable cause) {
        super.onWebSocketError(cause);
        logger.severe("WebSocket error in instance with remote \"%s\": %s".formatted(getRemote().getRemoteAddress(), cause));
    }

    public void send(String s) {
        try {
            getRemote().sendString(s);
        } catch (IOException e) {
            logger.severe("Sending \"%s\" through instance with remote \"%s\" failed with %s".formatted(s, getRemote().getRemoteAddress(), e));
            return;
        }
        logger.finest("Sent \"%s\" through instance with remote \"%s\"".formatted(s, getRemote().getRemoteAddress()));
    }
}
