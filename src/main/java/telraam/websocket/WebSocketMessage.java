package telraam.websocket;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor
public class WebSocketMessage<T> {
    private String topic;
    private T data;
}
