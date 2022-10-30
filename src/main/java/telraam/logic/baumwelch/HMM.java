package telraam.logic.baumwelch;

import com.google.common.collect.Lists;

import java.util.*;

public class HMM<S, O> {
    private final List<S> states;
    private final List<O> observationStates;

    private final Map<S, Map<S, Double>> transitionProbabilities = new HashMap<>();
    private final Map<S, Map<O, Double>> emissionProbabilities = new HashMap<>();

    public HMM(List<S> states, List<O> observationStates) {
        this.states = states;
        this.observationStates = observationStates;

        for (S s1 : states) {
            Map<S, Double> sTransitionProbabilities = new HashMap<>();
            transitionProbabilities.put(s1, sTransitionProbabilities);

            /* Initialize Transition Probabilities */
            double sum = 0;
            for (S s2 : states) {
                double probability = new Random().nextDouble();
                sTransitionProbabilities.put(s2, probability);
                sum += probability;
            }
            for (S s2 : states) {
                sTransitionProbabilities.put(s2, sTransitionProbabilities.get(s2) / sum);
            }

            /* Initialize Emission Probabilities */
            Map<O, Double> sEmissionProbabilities = new HashMap<>();
            emissionProbabilities.put(s1, sEmissionProbabilities);
            double probability = 1. / observationStates.size();

            for (O o : observationStates) {
                sEmissionProbabilities.put(o, probability);
            }
        }
    }

    public Map<S, Map<S, Double>> getTransitionProbabilities() {
        return transitionProbabilities;
    }

    public Map<S, Map<O, Double>> getEmissionProbabilities() {
        return emissionProbabilities;
    }

    public List<Map<S, Double>> forward(List<O> observations, Map<S, Double> startProbabilities, S endState) {
        LinkedList<Map<S, Double>> forwardHistory = new LinkedList<>(List.of(startProbabilities));

        for (O observation : observations.subList(1, observations.size())) {
            Map<S, Double> current = new HashMap<>();
            for (S state : states) {
                double sum = states.stream().map(
                        ps -> forwardHistory.getLast().get(ps) * transitionProbabilities.get(ps).get(state)
                ).mapToDouble(Double::doubleValue).sum();

                current.put(state, emissionProbabilities.get(state).get(observation) * sum);
            }
            forwardHistory.addLast(current);
        }
        return forwardHistory;
    }

    public List<Map<S, Double>> backward(List<O> observations, Map<S, Double> startProbabilities, S endState) {
        LinkedList<Map<S, Double>> backwardHistory = new LinkedList<>(List.of(new HashMap<>()));
        for (S state : states) {
            backwardHistory.getLast().put(state, transitionProbabilities.get(state).get(endState));
        }

        for (O observation : Lists.reverse(observations.subList(0, observations.size() - 1))) {
            Map<S, Double> current = new HashMap<>();
            for (S state : states) {
                double sum = states.stream().map(
                        ps -> transitionProbabilities.get(state).get(ps) * emissionProbabilities.get(ps).get(observation) * backwardHistory.getFirst().get(ps)
                ).mapToDouble(Double::doubleValue).sum();

                current.put(state, sum);
            }
            backwardHistory.addFirst(current);
        }
        return backwardHistory;
    }

    public List<Map<S, Map<S, Double>>> xiProbabilities(
            List<O> observations,
            List<Map<S, Double>> forwardHistory,
            List<Map<S, Double>> backwardHistory) {
        List<Map<S, Map<S, Double>>> siHistory = new ArrayList<>();
        for (int i = 0; i < observations.size() - 1; i++) {
            Map<S, Map<S, Double>> current = new HashMap<>();
            for (S state1 : states) {
                Map<S, Double> currentS = new HashMap<>();
                for (S state2 : states) {

                    double numerator = forwardHistory.get(i).get(state1) * transitionProbabilities.get(state1).get(state2);
                    numerator *= backwardHistory.get(i + 1).get(state2) * emissionProbabilities.get(state2).get(observations.get(i + 1));

                    double denominator = 0;

                    for (S k : states) {
                        for (S w : states) {
                            double partialDenominator = forwardHistory.get(i).get(k) * transitionProbabilities.get(k).get(w);
                            partialDenominator *= backwardHistory.get(i + 1).get(w) * emissionProbabilities.get(w).get(observations.get(i + 1));

                            denominator += partialDenominator;
                        }
                    }

                    currentS.put(state2, numerator / denominator);
                }
                current.put(state1, currentS);
            }
            siHistory.add(current);
        }
        return siHistory;
    }

    public List<Map<S, Double>> gammaProbabilities(
            List<O> observations,
            List<Map<S, Double>> forwardHistory,
            List<Map<S, Double>> backwardHistory) {
        List<Map<S, Double>> gammaHistory = new ArrayList<>();
        for (int i = 0; i < observations.size(); i++) {
            Map<S, Double> current = new HashMap<>();
            for (S state1 : states) {

                double numerator = forwardHistory.get(i).get(state1) * backwardHistory.get(i).get(state1);
                double denominator = 0;

                for (S state2 : states) {
                    denominator += forwardHistory.get(i).get(state2) * backwardHistory.get(i).get(state2);
                }

                current.put(state1, numerator / denominator);
            }
            gammaHistory.add(current);
        }
        return gammaHistory;
    }

    public void baumWelch(List<O> observations, Map<S, Double> startProbabilities, S endState) {

        List<Map<S, Double>> forwardHistory = forward(observations, startProbabilities, endState);
        List<Map<S, Double>> backwardHistory = backward(observations, startProbabilities, endState);

        List<Map<S, Double>> gammaHistory = gammaProbabilities(observations, forwardHistory, backwardHistory);
        List<Map<S, Map<S, Double>>> xiHistory = xiProbabilities(observations, forwardHistory, backwardHistory);

        for (S state1 : states) {
            for (S state2 : states) {
                double numerator = 0;
                double denominator = 0;
                for (int i = 0; i < observations.size()-1; i++) {
                    numerator += xiHistory.get(i).get(state1).get(state2);
                    denominator += gammaHistory.get(i).get(state1);
                }
                transitionProbabilities.get(state1).put(state2, numerator / denominator);
            }
        }

        for (S state1 : states) {
            for (O observationState : observationStates) {
                double numerator = 0;
                double denominator = 0;
                for (int i = 0; i < observations.size(); i++) {
                    if (observationState == observations.get(i)) {
                        numerator += gammaHistory.get(i).get(state1);
                    }
                    denominator += gammaHistory.get(i).get(state1);
                }
                emissionProbabilities.get(state1).put(observationState, numerator / denominator);
            }
        }
    }
}
