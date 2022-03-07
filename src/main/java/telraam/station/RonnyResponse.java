package telraam.station;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class RonnyResponse {
    private List<Detection> detections;

    @JsonProperty("station_id")
    private String stationId;

    public RonnyResponse(List<Detection> detections, String stationId) {
        this.detections = detections;
        this.stationId = stationId;
    }

    public RonnyResponse() {

    }

    public List<Detection> getDetections() {
        return this.detections;
    }

    public String getStationId() {
        return this.stationId;
    }
}
