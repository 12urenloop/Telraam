package telraam.station;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Detections {
    private List<Detection> detections;

    @JsonProperty("station_id")
    private String stationId;

    public Detections(List<Detection> detections, String stationId) {
        this.detections = detections;
        this.stationId = stationId;
    }

    public Detections() {

    }

    public List<Detection> getDetections() {
        return this.detections;
    }

    public String getStationId() {
        return this.stationId;
    }
}
