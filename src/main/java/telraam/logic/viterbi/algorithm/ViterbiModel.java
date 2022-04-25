package telraam.logic.viterbi.algorithm;

import java.util.Map;
import java.util.Set;

public record ViterbiModel<H, O>(Set<O> observations, Set<H> hiddenStates,
                                 Map<H, Map<H, Double>> transitionProbabilities,
                                 Map<H, Map<O, Double>> emitProbabilities,
                                 Map<H, Double> startProbabilities) {

    public Set<O> getObservations() {
        return observations;
    }

    public Set<H> getHiddenStates() {
        return hiddenStates;
    }

    public Map<H, Map<H, Double>> getTransitionProbabilities() {
        return transitionProbabilities;
    }

    public Map<H, Map<O, Double>> getEmitProbabilities() {
        return emitProbabilities;
    }

    public Map<H, Double> getStartProbabilities() {
        return startProbabilities;
    }
}
