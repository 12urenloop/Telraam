package telraam.database.models;

import java.sql.Timestamp;
import java.util.Objects;

public class LapSourceSwitchover {
    private Integer id;
    private Integer newLapSource;
    private Timestamp timestamp;

    // DO NOT REMOVE
    public LapSourceSwitchover() {}

    public LapSourceSwitchover(Integer newLapSource, Timestamp timestamp) {
        this.newLapSource = newLapSource;
        this.timestamp = timestamp;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getNewLapSource() {
        return newLapSource;
    }

    public void setNewLapSource(Integer newLapSource) {
        this.newLapSource = newLapSource;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        LapSourceSwitchover that = (LapSourceSwitchover) o;

        if (!Objects.equals(id, that.id)) return false;
        if (!Objects.equals(newLapSource, that.newLapSource)) return false;
        return Objects.equals(timestamp, that.timestamp);
    }

    @Override
    public int hashCode() {
        int result = id != null ? id.hashCode() : 0;
        result = 31 * result + (newLapSource != null ? newLapSource.hashCode() : 0);
        result = 31 * result + (timestamp != null ? timestamp.hashCode() : 0);
        return result;
    }
}
