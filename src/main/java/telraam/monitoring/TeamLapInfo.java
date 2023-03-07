package telraam.monitoring;

import com.fasterxml.jackson.annotation.JsonProperty;

public class TeamLapInfo {
    @JsonProperty("lap_time")
    private long lapTime;
    @JsonProperty("timestamp")
    private long timestamp;
    @JsonProperty("team_id")
    private int teamId;
    @JsonProperty("team_name")
    private String teamName;

    public TeamLapInfo(long lapTime, long timestamp, int teamId, String teamName) {
        this.lapTime = lapTime;
        this.timestamp = timestamp;
        this.teamId = teamId;
        this.teamName = teamName;
    }
}
