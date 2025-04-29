package telraam.station.websocket;

import jakarta.websocket.*;
import org.eclipse.jetty.websocket.core.exception.WebSocketTimeoutException;

import java.net.URI;

@ClientEndpoint
public class WebsocketClient {
    public interface MessageHandler {
        void handleMessage(String message);
    }
    public interface onStateChangeHandler {
        void handleChange();
    }

    private URI endpoint;
    private Session session = null;
    private MessageHandler messageHandler;
    private onStateChangeHandler onOpenHandler;
    private onStateChangeHandler onCloseHandler;
    private onStateChangeHandler onErrorHandler;
    private WebSocketContainer container;

    public WebsocketClient(URI endpointURI) throws RuntimeException {
        this.endpoint = endpointURI;
        container = ContainerProvider.getWebSocketContainer();
        container.setDefaultMaxTextMessageBufferSize(100 * 1048576);  // 100Mb
        container.setDefaultMaxSessionIdleTimeout(60000);
    }

    public void listen() throws  RuntimeException {
        try {
            container.connectToServer(this, endpoint);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @OnError
    public void onError(Session session, Throwable error) throws Throwable {
        if (this.onErrorHandler != null) {
            this.onErrorHandler.handleChange();
        }
        System.out.printf("Websocket get error %s%n", error);
        session.close();
        if (error instanceof WebSocketTimeoutException) {
            return;
        }
        throw error;
    }

    @OnOpen
    public void onOpen(Session session) {
        this.session = session;
        if (this.onOpenHandler != null) {
            this.onOpenHandler.handleChange();
        }
    }

    @OnClose
    public void onClose(Session userSession, CloseReason reason) {
        this.session = null;
        System.out.printf("Websocket get closed: %s%n", reason);
        if (this.onCloseHandler != null) {
            this.onCloseHandler.handleChange();
        }
    }

    @OnMessage
    public void onMessage(String message) {
        if (this.messageHandler != null) {
            this.messageHandler.handleMessage(message);
        }
    }

    public void addOnOpenHandler(onStateChangeHandler openHandler) {
        this.onOpenHandler = openHandler;
    }

    public void addOnCloseHandler(onStateChangeHandler openHandler) {
        this.onCloseHandler = openHandler;
    }

    public void addOnErrorHandler(onStateChangeHandler errorHandler) {
        this.onErrorHandler = errorHandler;
    }

    public void addMessageHandler(MessageHandler msgHandler) {
        this.messageHandler = msgHandler;
    }

    public void sendMessage(String message) {

        this.session.getAsyncRemote().sendText(message);
    }
}
