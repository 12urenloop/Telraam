package telraam.station;

import com.fasterxml.jackson.annotation.JsonProperty;

public class RonnyDetection {
    public int id;
    public String mac;
    public int rssi;
    public float battery;
    @JsonProperty("uptime_ms")
    public long uptimeMs;
    @JsonProperty("detection_timestamp")
    public long detectionTimestamp;
}
