package telraam.database.models;

import com.fasterxml.jackson.annotation.JsonProperty;

public class TeamLapCount {
    private Integer lapSourceId;
    private Integer lapCount;

    public TeamLapCount() {
    }

    public TeamLapCount(Integer lapTeamId, Integer lapCount) {
        this.lapSourceId = lapTeamId;
        this.lapCount = lapCount;
    }

    public Integer getLapSourceId() {
        return lapSourceId;
    }

    public void setLapSourceId(Integer lapSourceId) {
        this.lapSourceId = lapSourceId;
    }

    public Integer getLapCount() {
        return lapCount;
    }

    public void setLapCount(Integer lapCount) {
        this.lapCount = lapCount;
    }
}
