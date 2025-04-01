package telraam.logic.positioner;

import telraam.websocket.WebSocketMessage;
import telraam.websocket.WebSocketMessageSingleton;

import java.util.List;
import java.util.Map;

public class PositionSender {
    private final WebSocketMessage<List<Position>> message = new WebSocketMessage<>();
    private final String name;

    public PositionSender(String name) {
        this.message.setTopic("position");
        this.name = name;
    }

    public void send(List<Position> positions) {
        Map<String, Object> data = Map.of(
                "positioner", this.name,
                "positions", positions
        );
        WebSocketMessageSingleton.getInstance().sendToAll(this.message);
    }

}
