package telraam.station;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

public class RonnyDetection {
    private int id;
    private String mac;
    private int rssi;
    private float battery;
    @JsonProperty("uptime_ms")
    private long uptimeMs;
    @JsonProperty("detection_timestamp")
    private long detectionTimestamp;
    @JsonIgnore
    private String stationRonnyName;
    @JsonIgnore
    private int stationId;

    public int getId() {
        return this.id;
    }

    public String getMac() {
        return mac;
    }

    public int getRssi() {
        return rssi;
    }

    public float getBattery() {
        return battery;
    }

    public long getUptimeMs() {
        return uptimeMs;
    }

    public long getDetectionTimestamp() {
        return detectionTimestamp;
    }

    public int getStationId() {
        return stationId;
    }

    public String getStationRonnyName() {
        return stationRonnyName;
    }

    public void setStationRonnyName(String stationRonnyName) {
        this.stationRonnyName = stationRonnyName;
    }

    public void setStationId(int id) { this.stationId = id; }
}
