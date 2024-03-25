package telraam.station.models;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class RonnyResponse {
    public List<RonnyDetection> detections;

    @JsonProperty("station_id")
    public String stationRonnyName;
}
