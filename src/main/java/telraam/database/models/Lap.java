package telraam.database.models;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.sql.Timestamp;

@Getter @Setter @NoArgsConstructor
public class Lap {
    private Integer id;
    private Integer teamId;
    private Integer lapSourceId;

    private Boolean manual;
    private Timestamp timestamp;

    public Lap(Integer teamId, Integer lapSourceId, Timestamp timestamp) {
        this.teamId = teamId;
        this.lapSourceId = lapSourceId;
        this.timestamp = timestamp;
    }
}
