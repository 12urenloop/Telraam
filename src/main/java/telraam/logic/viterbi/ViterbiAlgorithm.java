package telraam.logic.viterbi;

import java.util.*;

/**
 * The class performing the Viterbi algorithm.
 * @param <H> The type of the hidden states.
 * @param <O> The type of the observations.
 */
public class ViterbiAlgorithm<H, O> {
    private final Set<O> observations;
    private final Set<H> hiddenStates;
    private final Map<H, Map<H, Double>> transitionProbabilities;
    private final Map<H, Map<O, Double>> emitProbabilities;

    private Result<H> lastResult;

    public ViterbiAlgorithm(
            Set<O> observations,
            Set<H> hiddenStates,
            Map<H, Map<H, Double>> transitionProbabilities,
            Map<H, Map<O, Double>> emitProbabilities,
            Map<H, Double> startProbabilities
    ) {
        this.observations = observations;
        this.hiddenStates = hiddenStates;
        this.transitionProbabilities = transitionProbabilities;
        this.emitProbabilities = emitProbabilities;

        this.verifyProbabilities();

        // Set up the initial probabilities
        this.lastResult = new Result<>(null);
        for (Map.Entry<H, Double> entry : startProbabilities.entrySet()) {
            this.lastResult.setProbability(entry.getKey(), entry.getValue());
            this.lastResult.setPreviousState(entry.getKey(), null);
        }
    }

    /**
     * Verify that the given probabilities are valid.
     * @throws InvalidParameterException If the probabilities were not valid.
     */
    private void verifyProbabilities() {
        if (!transitionProbabilities.keySet().equals(this.hiddenStates)) {
            throw new InvalidParameterException("Invalid key set for transition probabilities");
        }

        for (H state : this.hiddenStates) {
            if (!transitionProbabilities.get(state).keySet().equals(this.hiddenStates)) {
                throw new InvalidParameterException("Invalid key set for transition probabilities for state " + state);
            }
        }

        if (!emitProbabilities.keySet().equals(this.hiddenStates)) {
            throw new InvalidParameterException("Invalid key set for emission probabilities");
        }

        for (H state : this.hiddenStates) {
            if (!transitionProbabilities.get(state).keySet().equals(this.observations)) {
                throw new InvalidParameterException("Invalid key set for emission probabilities for state " + state);
            }
        }
    }

    /**
     * Handle an observation.
     * @param observation The observation to process.
     */
    public void observe(O observation) {
        Result<H> newResult = new Result<>(this.lastResult);
        for (H nextState : this.hiddenStates) {
            for (H previousState : this.hiddenStates) {
                newResult.addProbability(
                        nextState,
                        previousState,
                        this.lastResult.getProbability(previousState) *
                                this.transitionProbabilities.get(previousState).get(nextState) *
                                this.emitProbabilities.get(nextState).get(observation)
                );
            }
        }

        newResult.normalize();

        this.lastResult = newResult;

        //TODO: only keep the last X Results, as there will be a LOT of observations.
    }

    /**
     * Get the current state of the Viterbi algorithm.
     * @return The last Result.
     */
    public Result<H> getResult() {
        return this.lastResult;
    }
}
