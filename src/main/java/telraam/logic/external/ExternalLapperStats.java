package telraam.logic.external;

import java.util.List;

public class ExternalLapperStats {
    private List<Double> errorHistory;
    private List<List<Double>> transitionMatrix;
    private List<List<Double>> emissionMatrix;

    public List<Double> getErrorHistory() {
        return errorHistory;
    }

    public void setErrorHistory(List<Double> errorHistory) {
        this.errorHistory = errorHistory;
    }

    public List<List<Double>> getTransitionMatrix() {
        return transitionMatrix;
    }

    public void setTransitionMatrix(List<List<Double>> transitionMatrix) {
        this.transitionMatrix = transitionMatrix;
    }

    public List<List<Double>> getEmissionMatrix() {
        return emissionMatrix;
    }

    public void setEmissionMatrix(List<List<Double>> emissionMatrix) {
        this.emissionMatrix = emissionMatrix;
    }
}
