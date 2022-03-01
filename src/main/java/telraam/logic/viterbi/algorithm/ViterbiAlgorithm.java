package telraam.logic.viterbi.algorithm;

import java.util.*;

/**
 * The class performing the Viterbi algorithm.
 * @param <H> The type of the hidden states.
 * @param <O> The type of the observations.
 */
public class ViterbiAlgorithm<H, O> {
    private final ViterbiModel<H, O> model;

    private Result<H> lastResult;

    public ViterbiAlgorithm(ViterbiModel<H, O> viterbiModel) {
        this.model = viterbiModel;

        System.out.println("Starting Viterbi algorithm with observations " + viterbiModel.getObservations() + " and hidden states " + viterbiModel.getHiddenStates());

        this.verifyProbabilities();

        // Set up the initial probabilities
        this.lastResult = new Result<>(null);
        for (Map.Entry<H, Double> entry : viterbiModel.getStartProbabilities().entrySet()) {
            this.lastResult.setProbability(entry.getKey(), entry.getValue());
            this.lastResult.setPreviousState(entry.getKey(), null);
        }
    }

    /**
     * Verify that the given probabilities are valid.
     * @throws InvalidParameterException If the probabilities were not valid.
     */
    private void verifyProbabilities() {
        if (!this.model.getTransitionProbabilities().keySet().equals(this.model.getHiddenStates())) {
            throw new InvalidParameterException("Invalid key set for transition probabilities");
        }

        for (H state : this.model.getHiddenStates()) {
            if (!this.model.getTransitionProbabilities().get(state).keySet().equals(this.model.getHiddenStates())) {
                throw new InvalidParameterException("Invalid key set for transition probabilities for state " + state);
            }
        }

        if (!this.model.getEmitProbabilities().keySet().equals(this.model.getHiddenStates())) {
            throw new InvalidParameterException("Invalid key set for emission probabilities: " + this.model.getEmitProbabilities().keySet() + " != " + this.model.getHiddenStates());
        }

        for (H state : this.model.getHiddenStates()) {
            if (!this.model.getTransitionProbabilities().get(state).keySet().equals(this.model.getHiddenStates())) {
                throw new InvalidParameterException(
                        "Invalid key set for emission probabilities for state " +
                        state +
                        ": " +
                        this.model.getTransitionProbabilities().get(state).keySet() +
                        " != " +
                        this.model.getObservations()
                );
            }
        }
    }

    /**
     * Handle an observation.
     * @param observation The observation to process.
     */
    public void observe(O observation) {
        Result<H> newResult = new Result<>(this.lastResult);
        for (H nextState : this.model.getHiddenStates()) {
            for (H previousState : this.model.getHiddenStates()) {
                newResult.addProbability(
                        nextState,
                        previousState,
                        this.lastResult.getProbability(previousState) *
                                this.model.getTransitionProbabilities().get(previousState).get(nextState) *
                                this.model.getEmitProbabilities().get(nextState).get(observation)
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
