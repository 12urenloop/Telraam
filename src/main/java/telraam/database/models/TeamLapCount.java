package telraam.database.models;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor @AllArgsConstructor
public class TeamLapCount {
    private Integer lapSourceId;
    private Integer lapCount;
}
