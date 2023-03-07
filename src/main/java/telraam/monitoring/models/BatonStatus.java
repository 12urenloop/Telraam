package telraam.monitoring.models;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.sql.Timestamp;

public class BatonStatus {
    private String mac;
    private Integer id;
    private String name;
    private Float battery;
    // Uptime in seconds
    private Long uptime;
    private Boolean rebooted;
    @JsonProperty("time_since_seen")
    private Long lastSeenSecondsAgo;
    @JsonIgnore
    private Timestamp lastSeen;
    @JsonProperty("last_detected_at_station")
    private Integer lastDetectedAtStation;

    public BatonStatus(String mac, Integer id, String name, float battery, long uptime, boolean rebooted, Timestamp lastSeen, Integer LDAS) {
        this.mac = mac;
        this.id = id;
        this.name = name;
        this.battery = battery;
        this.uptime = uptime;
        this.rebooted = rebooted;
        this.lastSeen = lastSeen;
        this.lastSeenSecondsAgo = lastSeen != null ? (System.currentTimeMillis() - lastSeen.getTime()) / 1000 : null;
        this.lastDetectedAtStation = LDAS;
    }

    // Getters and setters
    public String getMac() {
        return mac;
    }

    public void setMac(String mac) {
        this.mac = mac;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Float getBattery() {
        return battery;
    }

    public void setBattery(Float battery) {
        this.battery = battery;
    }

    public Long getUptime() {
        return uptime;
    }

    public void setUptime(Long uptime) {
        this.uptime = uptime;
    }

    public Boolean getRebooted() {
        return rebooted;
    }

    public void setRebooted(Boolean rebooted) {
        this.rebooted = rebooted;
    }

    public Long getLastSeenSecondsAgo() {
        return lastSeenSecondsAgo;
    }

    public void setLastSeenSecondsAgo(Long lastSeenSecondsAgo) {
        this.lastSeenSecondsAgo = lastSeenSecondsAgo;
    }

    public Timestamp getLastSeen() {
        return lastSeen;
    }

    public void setLastSeen(Timestamp lastSeen) {
        this.lastSeen = lastSeen;
    }

    public Integer getLastDetectedAtStation() {
        return lastDetectedAtStation;
    }

    public void setLastDetectedAtStation(Integer lastDetectedAtStation) {
        this.lastDetectedAtStation = lastDetectedAtStation;
    }

    @Override
    public String toString() {
        return String.format("BatonStatus{mac='%s', id=%d, name='%s', battery=%f, uptime=%d, rebooted=%b, lastSeen=%s, lastSeenSecondsAgo=%d, lastDetectedAtStation=%d}",
                mac, id, name, battery, uptime, rebooted, lastSeen, lastSeenSecondsAgo, lastDetectedAtStation);
    }
}
