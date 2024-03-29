package telraam.monitoring.models;

import com.fasterxml.jackson.annotation.JsonProperty;

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

    public BatonDetection(Integer detectionTime, Integer rssi, Integer stationId, Integer teamId, String teamName) {
        this.detectionTime = detectionTime;
        this.rssi = rssi;
        this.stationId = stationId;
        this.teamId = teamId;
        this.teamName = teamName;
    }

    public Integer getDetectionTime() {
        return detectionTime;
    }

    public void setDetectionTime(Integer detectionTime) {
        this.detectionTime = detectionTime;
    }

    public Integer getRssi() {
        return rssi;
    }

    public void setRssi(Integer rssi) {
        this.rssi = rssi;
    }

    public Integer getTeamId() {
        return teamId;
    }

    public void setTeamId(Integer teamId) {
        this.teamId = teamId;
    }

    public Integer getStationId() {
        return stationId;
    }

    public void setStationId(Integer stationId) {
        this.stationId = stationId;
    }

    public String getTeamName() {
        return teamName;
    }

    public void setTeamName(String teamName) {
        this.teamName = teamName;
    }
}
