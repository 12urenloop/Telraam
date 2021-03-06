package telraam.database.models;

import java.sql.Timestamp;

public class Lap {
    private Integer id;
    private Integer teamId;
    private Timestamp timestamp;

    public Lap() {
    }

    public Lap(Integer teamId, Timestamp timestamp) {
        this.teamId = teamId;
        this.timestamp = timestamp;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getTeamId() {
        return teamId;
    }

    public void setTeamId(Integer teamId) {
        this.teamId = teamId;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }
}