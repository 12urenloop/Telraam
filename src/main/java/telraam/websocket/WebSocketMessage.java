package telraam.websocket;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter
public class WebSocketMessage<T> {
    private String topic;
    private T data;
}
