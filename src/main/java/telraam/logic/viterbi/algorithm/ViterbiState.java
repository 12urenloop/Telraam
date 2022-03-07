package telraam.logic.viterbi.algorithm;

import java.util.Arrays;
import java.util.Comparator;
import java.util.Map;
import java.util.Optional;

/**
 * Helper class to store steps in the Viterbi algorithm.
 */
public class ViterbiState {
    private final double[] probabilities;
    private final int[] previousStates;
    private final int[] lapCounts;
    private final ViterbiState previousState;

    public ViterbiState(ViterbiState previousState, double[] probabilities, int[] previousStates, int[] lapCounts) {
        this.previousState = previousState;
        this.probabilities = probabilities;
        this.previousStates = previousStates;
        this.lapCounts = lapCounts;
    }

    /**
     * Get the probability of being in a given hidden state.
     * @param state The state to observe.
     * @return The probability.
     */
    public double getProbability(int state) {
        return this.probabilities[state];
    }

    /**
     * Get the probabilities for each hidden state.
     * @return The probabilities.
     */
    public double[] getProbabilities() {
        return this.probabilities;
    }

    /**
     * Get the previous ViterbiState.
     * @return The previous ViterbiState.
     */
    public ViterbiState getPreviousState() {
        return previousState;
    }

    /**
     * Get the most likely previous hidden state for a given hidden state.
     * @param state The state to look up.
     * @return The most likely previous state.
     */
    public int getPreviousSegment(int state) {
        return this.previousStates[state];
    }

    /**
     * Get the most likely state to be in, in this Result
     * @return The state that is most likely.
     */
    public Integer mostLikelyState() {
        int mostLikelyState = 0;
        for (int i = 1; i < this.probabilities.length; i++) {
            if (this.probabilities[i] > this.probabilities[mostLikelyState]) {
                mostLikelyState = i;
            }
        }
        return mostLikelyState;
    }
}
