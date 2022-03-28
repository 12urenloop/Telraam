package telraam.database.models;

import java.sql.Timestamp;

public class Detection {
    private Integer id;
    private Integer batonId;
    private Integer stationId;
    private Timestamp timestamp;

    public Detection() {
    }

    public Detection(Integer batonId, Integer stationId, Timestamp timestamp) {
        this.batonId = batonId;
        this.stationId = stationId;
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

    public Integer getStationId() {
        return stationId;
    }

    public void setStationId(Integer stationId) {
        this.stationId = stationId;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }
}
