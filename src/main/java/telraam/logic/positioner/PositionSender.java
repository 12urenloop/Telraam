package telraam.logic.positioner;

import telraam.websocket.WebSocketMessage;
import telraam.websocket.WebSocketMessageSingleton;

import java.util.List;

public class PositionSender {
    private final WebSocketMessage<List<Position>> message = new WebSocketMessage<>();

    public PositionSender() {
        this.message.setTopic("position");
    }

    public void send(List<Position> positions) {
        this.message.setData(positions);
        WebSocketMessageSingleton.getInstance().sendToAll(this.message);
    }

}
