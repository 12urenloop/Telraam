package telraam.station.websocket;

import jakarta.websocket.*;

import java.net.URI;

@ClientEndpoint
public class WebsocketClient {
    public interface MessageHandler {
        void handleMessage(String message);
    }
    public interface OnOpenHandler {
        void handleMsgOpen();
    }

    Session session = null;
    private MessageHandler messageHandler;
    private OnOpenHandler onOpenHandler;

    public WebsocketClient(URI endpointURI) throws RuntimeException {
        try {
            WebSocketContainer container = ContainerProvider.getWebSocketContainer();
            container.connectToServer(this, endpointURI);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @OnOpen
    public void onOpen(Session session) {
        this.session = session;
    }

    @OnClose
    public void onClose(Session userSession, CloseReason reason) {
        System.out.println("closing websocket");
        this.session = null;
    }

    @OnMessage
    public void onMessage(String message) {
        if (this.messageHandler != null) {
            this.messageHandler.handleMessage(message);
        }
    }

    public void addOnOpenHandler(OnOpenHandler openHandler) {
        this.onOpenHandler = openHandler;
    }

    public void addMessageHandler(MessageHandler msgHandler) {
        this.messageHandler = msgHandler;
    }

    public void sendMessage(String message) {
        this.session.getAsyncRemote().sendText(message);
    }
}
