package telraam.logic.viterbi.algorithm;

import io.swagger.models.auth.In;

import java.util.*;

/**
 * The class performing the Viterbi algorithm.
 * @param <O> The type of the observations.
 */
public class ViterbiAlgorithm<O> {
    private final ViterbiModel<Integer, O> model;

    private ViterbiState lastState;

    public ViterbiAlgorithm(ViterbiModel<Integer, O> viterbiModel) {
        this.model = viterbiModel;

        this.verifyProbabilities();

        // Set up the initial probabilities
        int numSegments = this.model.getHiddenStates().size();
        Map<Integer, Double> probabilities = new HashMap<>();
        Map<Integer, Integer> previousSegments = new HashMap<>();
        Map<Integer, Integer> lapCounts = new HashMap<>();

        for (Map.Entry<Integer, Double> entry : viterbiModel.getStartProbabilities().entrySet()) {
            probabilities.put(entry.getKey(), entry.getValue());
            previousSegments.put(entry.getKey(), 0);
            lapCounts.put(entry.getKey(), 0);
        }

        this.lastState = new ViterbiState(probabilities, previousSegments, lapCounts);
    }

    /**
     * Verify that the given probabilities are valid.
     * @throws InvalidParameterException If the probabilities were not valid.
     */
    private void verifyProbabilities() {
        if (!this.model.getTransitionProbabilities().keySet().equals(this.model.getHiddenStates())) {
            throw new InvalidParameterException("Invalid key set for transition probabilities");
        }

        for (Integer state : this.model.getHiddenStates()) {
            if (!this.model.getTransitionProbabilities().get(state).keySet().equals(this.model.getHiddenStates())) {
                throw new InvalidParameterException("Invalid key set for transition probabilities for state " + state);
            }
        }

        if (!this.model.getEmitProbabilities().keySet().equals(this.model.getHiddenStates())) {
            throw new InvalidParameterException("Invalid key set for emission probabilities: " + this.model.getEmitProbabilities().keySet() + " != " + this.model.getHiddenStates());
        }

        for (Integer state : this.model.getHiddenStates()) {
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
        int numSegments = this.model.getHiddenStates().size();
        Map<Integer, Double> probabilities = new HashMap<>();
        Map<Integer, Integer> previousSegments = new HashMap<>();
        Map<Integer, Integer> lapCounts = new HashMap<>();

        for (int nextSegment = 0; nextSegment < numSegments; nextSegment++) {
            probabilities.put(nextSegment, 0.0);
            for (int previousSegment = 0; previousSegment < numSegments; previousSegment++) {
                double probability = this.lastState.probabilities().get(previousSegment) *
                        this.model.getTransitionProbabilities().get(previousSegment).get(nextSegment) *
                        this.model.getEmitProbabilities().get(nextSegment).get(observation);
                if (probabilities.get(nextSegment) < probability) {
                    probabilities.put(nextSegment, probability);
                    previousSegments.put(nextSegment, previousSegment);

                    int half = numSegments / 2;
                    // Dit is het algoritme van De Voerstreek
                    int delta = half - (half - (nextSegment - previousSegment)) % numSegments;

                    if (delta > 0 && previousSegment > nextSegment) {
                        // forward wrap-around
                        lapCounts.put(nextSegment, this.lastState.lapCounts().get(previousSegment) + 1);

                    } else if (delta < 0 && previousSegment < nextSegment) {
                        // backwards wrap-around
                        lapCounts.put(nextSegment, this.lastState.lapCounts().get(previousSegment) - 1);
                    } else {
                        // no wrap-around (c) robbe
                        lapCounts.put(nextSegment, this.lastState.lapCounts().get(previousSegment));
                    }
                }
            }
        }

        // normalize probabilities
        double sum = probabilities.values().stream().reduce(0.0, Double::sum);
        for (int i = 0; i < numSegments; i++) {
            probabilities.put(i, probabilities.get(i) / sum);
        }

        this.lastState = new ViterbiState(probabilities, previousSegments, lapCounts);
    }

    /**
     * Get the current state of the Viterbi algorithm.
     * @return The last Result.
     */
    public ViterbiState getState() {
        return this.lastState;
    }
}
