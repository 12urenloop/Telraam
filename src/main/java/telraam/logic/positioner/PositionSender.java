package telraam.logic.positioner;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.NoArgsConstructor;
import telraam.websocket.WebSocketMessage;
import telraam.websocket.WebSocketMessageSingleton;

import java.util.List;
import java.util.logging.Logger;

public class PositionSender {
    private static final Logger logger = Logger.getLogger(PositionSender.class.getName());
    private final ObjectMapper mapper = new ObjectMapper();
    private final WebSocketMessage message = new WebSocketMessage();

    public PositionSender() {
        this.message.setTopic("position");
    }

    public void send(List<Position> position) {
        try {
            String json = mapper.writeValueAsString(position);
            this.message.setData(json);
            WebSocketMessageSingleton.getInstance().sendToAll(this.message);
        } catch (JsonProcessingException e) {
            logger.severe("Json conversion error for \"%s\"".formatted(position.toString()));
        }
    }

}
