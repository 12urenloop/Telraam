package telraam.logic.viterbi.algorithm;

import java.sql.Timestamp;
import java.util.Map;
import java.util.Set;

/**
 * Helper class to store steps in the Viterbi algorithm.
 */
public record ViterbiState(Map<Integer, Double> probabilities, Map<Integer, Integer> previousStates, Map<Integer, Set<Timestamp>> lapTimestamps) {
    /**
     * Get the most likely state to be in, in this Result
     *
     * @return The state that is most likely.
     */
    public Integer mostLikelySegment() {
        int mostLikelySegment = 0;
        for (int i : probabilities.keySet()) {
            if (this.probabilities.get(i) > this.probabilities.get(mostLikelySegment)) {
                mostLikelySegment = i;
            }
        }
        return mostLikelySegment;
    }
}
