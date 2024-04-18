package telraam.logic.positioner;

import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import lombok.Getter;
import lombok.Setter;

@Getter @Setter @JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public class Position {
    private final int teamId;
    private double progress; // Progress of the lap. Between 0-1
    private double speed; // Current speed. progress / second
    private double timestamp;

    public Position(int teamId) {
        this.teamId = teamId;
        this.progress = 0;
        this.speed = 0;
        this.timestamp = 0;
    }

    public void update(double progress, double speed) {
        this.progress = progress;
        this.speed = speed;
        this.timestamp = System.currentTimeMillis() / 1000D;
    }
}
