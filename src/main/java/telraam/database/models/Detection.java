package telraam.database.models;

import java.sql.Timestamp;

public class Detection {
    private Integer id;
    private Integer batonId;
    private Integer beaconId;
    private Timestamp timestamp;

    public Detection() {
    }

    public Detection(Integer batonId, Integer beaconId, Timestamp timestamp) {
        this.batonId = batonId;
        this.beaconId = beaconId;
        this.timestamp = timestamp;
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

    public Integer getBeaconId() {
        return beaconId;
    }

    public void setBeaconId(Integer beaconId) {
        this.beaconId = beaconId;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }
}
