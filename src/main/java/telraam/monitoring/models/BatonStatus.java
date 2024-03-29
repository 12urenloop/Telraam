package telraam.monitoring.models;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import java.sql.Timestamp;

@Getter
@Setter
public class BatonStatus {

    private String mac;
    private Integer id;
    private String name;
    private Float battery;
    private Long uptime; // Uptime in seconds
    private Boolean rebooted;
    @JsonProperty("time_since_seen")
    private Long lastSeenSecondsAgo;
    @JsonIgnore
    private Timestamp lastSeen;
    @JsonProperty("last_detected_at_station")
    private Integer lastDetectedAtStation;

    public BatonStatus(String mac, Integer id, String name, float battery, long uptime, boolean rebooted, Timestamp lastSeen, Integer lastDetectedAtStation) {
        this.mac = mac;
        this.id = id;
        this.name = name;
        this.battery = battery;
        this.uptime = uptime;
        this.rebooted = rebooted;
        this.lastSeen = lastSeen;
        this.lastSeenSecondsAgo = lastSeen != null ? (System.currentTimeMillis() - lastSeen.getTime()) / 1000 : null;
        this.lastDetectedAtStation = lastDetectedAtStation;
    }

    @Override
    public String toString() {
        return String.format("BatonStatus{mac='%s', id=%d, name='%s', battery=%f, uptime=%d, rebooted=%b, lastSeen=%s, lastSeenSecondsAgo=%d, lastDetectedAtStation=%d}",
                mac, id, name, battery, uptime, rebooted, lastSeen, lastSeenSecondsAgo, lastDetectedAtStation);
    }
}
