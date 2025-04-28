package telraam.database.models;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter
public class Podium {
    private int teamId;
    private int rank;
    private int rounds;

    public Podium(int teamId, int rank, int rounds) {
        this.teamId = teamId;
        this.rank = rank;
        this.rounds = rounds;
    }
}
