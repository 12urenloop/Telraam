package telraam.database.models;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.sql.Timestamp;
import java.util.Objects;

@Getter @Setter @NoArgsConstructor
public class LapSourceSwitchover {
    private Integer id;
    private Integer newLapSource;
    private Timestamp timestamp;

    public LapSourceSwitchover(Integer newLapSource, Timestamp timestamp) {
        this.newLapSource = newLapSource;
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
