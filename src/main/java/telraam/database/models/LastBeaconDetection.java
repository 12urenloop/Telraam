package telraam.database.models;

import java.sql.Timestamp;

public class LastBeaconDetection {

    private Integer beaconId;
    private Timestamp timestamp;


    public LastBeaconDetection() {
    }


    public LastBeaconDetection(Integer beaconId, Timestamp timestamp) {
        this.beaconId = beaconId;
        this.timestamp = timestamp;
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
