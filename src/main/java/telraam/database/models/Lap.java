package telraam.database.models;

import java.sql.Timestamp;

public class Lap {
    private Integer id;
    private Integer team_id;
    private Timestamp timestamp;

    public Lap() {};

    public Lap(Integer team_id, Timestamp timestamp) {
        this.team_id = team_id;
        this.timestamp = timestamp;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getTeam_id() {
        return team_id;
    }

    public void setTeam_id(Integer team_id) {
        this.team_id = team_id;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }
}