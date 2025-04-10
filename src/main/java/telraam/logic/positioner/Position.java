package telraam.logic.positioner;

import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import lombok.Getter;
import lombok.Setter;

@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public record Position (
        int teamId,
        double progress,
        double speed,
        long timestamp
) {}
//public class Position {
//    private final int teamId;
//    private double progress; // Progress of the lap. Between 0-1
//    private double speed; // Current speed. progress / millisecond
//    private long timestamp; // Timestamp in milliseconds
//
//    public Position(int teamId) {
//        this.teamId = teamId;
//        this.progress = 0;
//        this.speed = 0;
//        this.timestamp = System.currentTimeMillis();
//    }
//
//    public Position(int teamId, double progress) {
//        this.teamId = teamId;
//        this.progress = progress;
//        this.speed = 0;
//        this.timestamp = System.currentTimeMillis();
//    }
//
//    public void update(double progress, double speed, long timestamp) {
//        this.progress = progress;
//        this.speed = speed;
//        this.timestamp = timestamp;
//    }
//}
