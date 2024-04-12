package telraam.logic.positioner;

import lombok.Getter;
import lombok.Setter;

import java.sql.Timestamp;

@Getter @Setter
public class Position {
    private int teamId;
    private double progress; // Progress of the lap. Between 0-1
    private double speed; // Current speed. progress / second
    private long unix;

    public Position(int teamId) {
        this.teamId = teamId;
        this.progress = 0;
        this.speed = 0;
        this.unix = 0;
    }

    public void update(double progress, double speed) {
        this.progress = progress;
        this.speed = speed;
        this.unix = System.currentTimeMillis() / 1000L;;
    }
}
