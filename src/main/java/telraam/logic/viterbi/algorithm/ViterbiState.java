package telraam.logic.viterbi.algorithm;

/**
 * Helper class to store steps in the Viterbi algorithm.
 */
public record ViterbiState(double[] probabilities, int[] previousStates, int[] lapCounts) {
    /**
     * Get the most likely state to be in, in this Result
     *
     * @return The state that is most likely.
     */
    public Integer mostLikelySegment() {
        int mostLikelySegment = 0;
        for (int i = 1; i < this.probabilities.length; i++) {
            if (this.probabilities[i] > this.probabilities[mostLikelySegment]) {
                mostLikelySegment = i;
            }
        }
        return mostLikelySegment;
    }
}
