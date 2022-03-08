package telraam.database.models;

import java.sql.Time;
import java.sql.Timestamp;

public class BatonAssignment {
    private Integer id;
    private Integer batonId;
    private Integer teamId;
    private Timestamp timestamp;

    public BatonAssignment() {
    }

    public BatonAssignment(int batonId, int teamId) {
        this.batonId = batonId;
        this.teamId = teamId;
    }

    public BatonAssignment(Baton baton, Team team) {
        this.batonId = baton.getId();
        this.teamId = team.getId();
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getBatonId() {
        return batonId;
    }

    public void setBatonId(Integer batonId) {
        this.batonId = batonId;
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
