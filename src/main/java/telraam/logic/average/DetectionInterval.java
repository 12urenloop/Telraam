package telraam.logic.average;

import java.sql.Timestamp;

public class DetectionInterval {
    private Integer count;
    private Timestamp endTime;

    public DetectionInterval(Timestamp endTime) {
        this.count = 1;
        this.endTime = endTime;
    }

    public Timestamp getEndTime() {
        return endTime;
    }

    public void setEndTime(Timestamp endTime) {
        this.endTime = endTime;
        count++;
    }

    public Integer getCount() {
        return count;
    }
}
