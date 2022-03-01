package telraam.logic.viterbi.algorithm;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * Helper class to store steps in the Viterbi algorithm.
 * @param <H> The type of the hidden states.
 */
public class Result<H> {
    private final Map<H, Double> probabilities;
    private final Map<H, H> previousStates;
    private final Result<H> previousResult;

    public Result(Result<H> previousResult) {
        this.previousResult = previousResult;
        this.probabilities = new HashMap<>();
        this.previousStates = new HashMap<>();
    }

    /**
     * Set the probability of one hidden state.
     * @param state The state to modify.
     * @param probability The probability of being in that state.
     */
    public void setProbability(H state, double probability) {
        this.probabilities.put(state, probability);
    }

    /**
     * Set the most likely previous hidden state for a given hidden state.
     * @param currentState The current hidden state.
     * @param previousState The most likely previous hidden state.
     */
    public void setPreviousState(H currentState, H previousState) {
        this.previousStates.put(currentState, previousState);
    }

    /**
     * Get the probability of being in a given hidden state.
     * @param state The state to observe.
     * @return The probability.
     */
    public double getProbability(H state) {
        return this.probabilities.get(state);
    }

    /**
     * Get the probabilities for each hidden state.
     * @return The probabilities.
     */
    public Map<H, Double> getProbabilities() {
        return this.probabilities;
    }

    /**
     * Get the previous Result.
     * @return The previous Result.
     */
    public Result<H> getPreviousResult() {
        return previousResult;
    }

    /**
     * Get the most likely previous hidden state for a given hidden state.
     * @param state The state to look up.
     * @return The most likely previous state.
     */
    public H getPreviousState(H state) {
        return this.previousStates.get(state);
    }

    /**
     * Get the most likely state to be in, in this Result
     * @return The state that is most likely.
     */
    public Optional<H> mostLikelyState() {
        return this.probabilities.entrySet()
                .stream()
                .max(Comparator.comparingDouble(Map.Entry::getValue))
                .map(Map.Entry::getKey);
    }

    /**
     * Normalize the probabilities.
     */
    public void normalize() {
        double total = this.probabilities.values().stream().reduce(Double::sum).orElse(1.0);
        this.probabilities.replaceAll((k, v) -> v / total);
    }

    /**
     * Add the probability of transitioning from one state to the next.
     * @param nextState The next (current) state.
     * @param previousState The previous state.
     * @param probability The transition probability.
     */
    public void addProbability(H nextState, H previousState, double probability) {
        double currentProbability = this.probabilities.getOrDefault(nextState, 0.0);
        if (probability > currentProbability) {
            this.previousStates.put(nextState, previousState);
        }
        this.probabilities.put(nextState, currentProbability + probability);
    }
}
