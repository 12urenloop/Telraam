package telraam.database.models;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.sql.Timestamp;
import java.util.Objects;

@Getter @Setter @NoArgsConstructor
public class BatonSwitchover {
    private Integer id;
    private Integer teamId;
    private Integer previousBatonId;
    private Integer newBatonId;
    private Timestamp timestamp;

    public BatonSwitchover(Integer teamId, Integer previousBatonId, Integer newBatonId, Timestamp timestamp) {
        this.teamId = teamId;
        this.previousBatonId = previousBatonId;
        this.newBatonId = newBatonId;
        this.timestamp = timestamp;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BatonSwitchover that = (BatonSwitchover) o;

        if (!Objects.equals(id, that.id)) return false;
        if (!Objects.equals(teamId, that.teamId)) return false;
        if (!Objects.equals(previousBatonId, that.previousBatonId))
            return false;
        return Objects.equals(newBatonId, that.newBatonId);
    }

    @Override
    public int hashCode() {
        int result = id != null ? id.hashCode() : 0;
        result = 31 * result + (teamId != null ? teamId.hashCode() : 0);
        result = 31 * result + (previousBatonId != null ? previousBatonId.hashCode() : 0);
        result = 31 * result + (newBatonId != null ? newBatonId.hashCode() : 0);
        return result;
    }
}
