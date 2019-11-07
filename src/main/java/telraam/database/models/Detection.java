package telraam.database.models;

import java.sql.Timestamp;

public class Detection {
    private Integer id;
    private Integer baton_id;
    private Integer beacon_id;
    private Timestamp timestamp;

    public Detection() {};

    public Detection(Integer baton_id, Integer beacon_id, Timestamp timestamp) {
        this.baton_id = baton_id;
        this.beacon_id = beacon_id;
        this.timestamp = timestamp;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getBaton_id() {
        return baton_id;
    }

    public void setBaton_id(Integer baton_id) {
        this.baton_id = baton_id;
    }

    public Integer getBeacon_id() {
        return beacon_id;
    }

    public void setBeacon_id(Integer beacon_id) {
        this.beacon_id = beacon_id;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }
}
