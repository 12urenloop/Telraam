package telraam.monitoring.models;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;

@AllArgsConstructor
public class TeamLapInfo {

    @JsonProperty("lap_time")
    private long lapTime;

    @JsonProperty("timestamp")
    private long timestamp;

    @JsonProperty("team_id")
    private int teamId;

    @JsonProperty("team_name")
    private String teamName;
}
