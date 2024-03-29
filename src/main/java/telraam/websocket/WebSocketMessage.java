package telraam.websocket;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter
public class WebSocketMessage {
    private String topic;
    private String data;
}
