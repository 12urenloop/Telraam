package telraam.station.websocket;

import jakarta.websocket.*;

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

    public WebsocketClient(URI endpointURI) throws RuntimeException {
        this.endpoint = endpointURI;
    }

    public void listen() throws  RuntimeException {
        try {
            WebSocketContainer container = ContainerProvider.getWebSocketContainer();
            container.setDefaultMaxTextMessageBufferSize(100 * 1048576);  // 100Mb
            container.setDefaultMaxSessionIdleTimeout(0);
            container.setAsyncSendTimeout(0);
            container.connectToServer(this, endpoint);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
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

    public void addMessageHandler(MessageHandler msgHandler) {
        this.messageHandler = msgHandler;
    }

    public void sendMessage(String message) {

        this.session.getAsyncRemote().sendText(message);
    }
}
