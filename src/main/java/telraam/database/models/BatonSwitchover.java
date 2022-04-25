package telraam.database.models;

import java.sql.Timestamp;
import java.util.Objects;

public class BatonSwitchover {
    private Integer id;
    private Integer teamId;
    private Integer previousBatonId;
    private Integer newBatonId;
    private Timestamp timestamp;

    // DO NOT REMOVE
    public BatonSwitchover() {}

    public BatonSwitchover(Integer teamId, Integer previousBatonId, Integer newBatonId, Timestamp timestamp) {
        this.teamId = teamId;
        this.previousBatonId = previousBatonId;
        this.newBatonId = newBatonId;
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

    public Integer getPreviousBatonId() {
        return previousBatonId;
    }

    public void setPreviousBatonId(Integer previousBatonId) {
        this.previousBatonId = previousBatonId;
    }

    public Integer getNewBatonId() {
        return newBatonId;
    }

    public void setNewBatonId(Integer newBatonId) {
        this.newBatonId = newBatonId;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BatonSwitchover that = (BatonSwitchover) o;

        if (!Objects.equals(id, that.id)) return false;
        if (!Objects.equals(teamId, that.teamId)) return false;
        if (!Objects.equals(previousBatonId, that.previousBatonId))
            return false;
        return Objects.equals(newBatonId, that.newBatonId);
    }

    @Override
    public int hashCode() {
        int result = id != null ? id.hashCode() : 0;
        result = 31 * result + (teamId != null ? teamId.hashCode() : 0);
        result = 31 * result + (previousBatonId != null ? previousBatonId.hashCode() : 0);
        result = 31 * result + (newBatonId != null ? newBatonId.hashCode() : 0);
        return result;
    }
}
