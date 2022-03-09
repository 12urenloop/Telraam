package telraam.station;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class RonnyResponse {
    private List<RonnyDetection> detections;

    @JsonProperty("station_id")
    private String stationRonnyName;

    public RonnyResponse(List<RonnyDetection> detections, String stationRonnyName) {
        this.detections = detections;
        this.stationRonnyName = stationRonnyName;
    }

    public RonnyResponse() {

    }

    public List<RonnyDetection> getDetections() {
        return this.detections;
    }

    public String getStationRonnyName() {
        return this.stationRonnyName;
    }

    public void setStationId(int stationId) {
        for (RonnyDetection detection : detections) {
            detection.setStationId(stationId);
        }
    }
}
