package telraam.database.models;

import java.sql.Timestamp;

public class Detection {
    private Integer id;
    private Integer batonId;
    private Integer stationId;
    private Integer rssi;
    private Float battery;
    private Long uptimeMs;
    private Integer remoteId;
    private Timestamp timestamp;
    private Timestamp timestampIngestion;

    public Detection() {
    }

    public Detection(Integer batonId, Integer stationId, Integer rssi, Float battery, Long uptimeMs, Integer remoteId, Timestamp timestamp, Timestamp timestampIngestion) {
        this.batonId = batonId;
        this.stationId = stationId;
        this.rssi = rssi;
        this.battery = battery;
        this.uptimeMs = uptimeMs;
        this.remoteId = remoteId;
        this.timestamp = timestamp;
        this.timestampIngestion = timestampIngestion;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getBatonId() {
        return batonId;
    }

    public void setBatonId(Integer batonId) {
        this.batonId = batonId;
    }

    public Integer getStationId() {
        return stationId;
    }

    public void setStationId(Integer stationId) {
        this.stationId = stationId;
    }

    public Integer getRssi() {
        return rssi;
    }

    public void setRssi(Integer rssi) {
        this.rssi = rssi;
    }

    public Float getBattery() {
        return battery;
    }

    public void setBattery(Float battery) {
        this.battery = battery;
    }

    public Long getUptimeMs() {
        return uptimeMs;
    }

    public void setUptimeMs(Long uptimeMs) {
        this.uptimeMs = uptimeMs;
    }

    public Integer getRemoteId() {
        return remoteId;
    }

    public void setRemoteId(Integer remoteId) {
        this.remoteId = remoteId;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }

    public Timestamp getTimestampIngestion() {
        return timestampIngestion;
    }

    public void setTimestampIngestion(Timestamp timestampIngestion) {
        this.timestampIngestion = timestampIngestion;
    }
}
