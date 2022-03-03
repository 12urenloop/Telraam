package telraam.station;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public record Detections(List<Detection> detections, @JsonProperty("station_id") String stationId) {
}
