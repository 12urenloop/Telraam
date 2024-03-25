package telraam.database.models;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.sql.Timestamp;

@Setter @Getter @NoArgsConstructor
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
    private Integer teamId;

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
}
