package telraam.database.models;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor
public class Team {
    private Integer id;
    private String name;
    private Integer batonId;

    public Team(String name) {
        this.name = name;
    }

    public Team(String name, int batonId) {
        this.name = name;
        this.batonId = batonId;
    }
}
