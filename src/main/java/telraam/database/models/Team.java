package telraam.database.models;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.Objects;

@Getter
@Setter
@NoArgsConstructor
public class Team {
    private Integer id;
    private String name;
    private Integer batonId;
    private String jacketNr = "INVALID";

    public Team(String name) {
        this.name = name;
    }

    public Team(String name, int batonId) {
        this.name = name;
        this.batonId = batonId;
    }

    public boolean equals(Team obj) {
        return Objects.equals(id, obj.getId());
    }
}
