package telraam.logic;

import org.jdbi.v3.core.Jdbi;
import telraam.database.models.Detection;
import telraam.logic.viterbi.ViterbiAlgorithm;

import java.util.Map;

public class ViterbiLapper implements Lapper {
    static final String SOURCE_NAME = "viterbi-lapper";

    private final Jdbi jdbi;
    private final Map<Integer, ViterbiAlgorithm<Integer, Integer>> viterbis;

    public ViterbiLapper(Jdbi jdbi) {
        this.jdbi = jdbi;
        this.viterbis = Map.of();
    }

    @Override
    public void handle(Detection msg) {
        this.viterbis.get(msg.getBatonId()).observe(msg.getBeaconId());
    }
}
