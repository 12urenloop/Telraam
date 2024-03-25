package telraam.station.models;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

public class RonnyResponse {
    public List<RonnyDetection> detections;

    @JsonProperty("station_id")
    public String stationRonnyName;
}
