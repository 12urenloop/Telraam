package telraam.monitoring.models;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
@AllArgsConstructor
public class BatonDetection {
    @JsonProperty("detected_time")
    private Integer detectionTime;
    @JsonProperty("rssi")
    private Integer rssi;
    @JsonProperty("team_id")
    private Integer teamId;
    @JsonProperty("station_id")
    private Integer stationId;
    @JsonProperty("team_name")
    private String teamName;
}
