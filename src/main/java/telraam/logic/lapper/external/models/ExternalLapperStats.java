package telraam.logic.lapper.external.models;

import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Setter @Getter
public class ExternalLapperStats {
    private List<Double> errorHistory;
    private List<List<Double>> transitionMatrix;
    private List<List<Double>> emissionMatrix;
}
