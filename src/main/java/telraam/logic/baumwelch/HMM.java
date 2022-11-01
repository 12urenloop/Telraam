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

    public List<Map<S, Double>> forward(List<O> observations, LinkedList<Double> scalingFactors, Map<S, Double> startProbabilities) {
        LinkedList<Map<S, Double>> forwardHistory = new LinkedList<>(List.of(startProbabilities));

        for (O observation : observations.subList(1, observations.size())) {
            Map<S, Double> current = new HashMap<>();
            double sum = 0.0;
            for (S state : states) {
                double total = states.stream().map(
                        ps -> forwardHistory.getLast().get(ps) * transitionProbabilities.get(ps).get(state)
                ).mapToDouble(Double::doubleValue).sum() * emissionProbabilities.get(state).get(observation);

                sum += total;
                current.put(state, total);
            }

            scalingFactors.addLast(sum);
            for (S state : states) {
                current.put(state, current.get(state) / sum);
            }

            forwardHistory.addLast(current);
        }
        return forwardHistory;
    }

    public List<Map<S, Double>> backward(List<O> observations, LinkedList<Double> scalingFactors) {
        LinkedList<Map<S, Double>> backwardHistory = new LinkedList<>(List.of(new HashMap<>()));
        for (S state : states) {
            /* TODO: verify the endState requirements */
            //backwardHistory.getLast().put(state, transitionProbabilities.get(state).get(endState));
            backwardHistory.getLast().put(state, 1.);
        }

        for (O observation : Lists.reverse(observations.subList(0, observations.size() - 1))) {
            Map<S, Double> current = new HashMap<>();
            for (S state : states) {
                double total = states.stream().map(
                        ps -> transitionProbabilities.get(state).get(ps) * emissionProbabilities.get(ps).get(observation) * backwardHistory.getFirst().get(ps)
                ).mapToDouble(Double::doubleValue).sum();

                current.put(state, total);
            }

            double scalingFactor = scalingFactors.removeFirst();
            for (S state : states) {
                current.put(state, current.get(state) / scalingFactor);
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

        ArrayList<Map<S, Double>> forwardHistoryArrayList = new ArrayList<>(forwardHistory);
        ArrayList<Map<S, Double>> backwardHistoryArrayList = new ArrayList<>(backwardHistory);

        for (int i = 0; i < observations.size() - 1; i++) {
            Map<S, Double> currentForward = forwardHistoryArrayList.get(i);
            Map<S, Double> currentBackward = backwardHistoryArrayList.get(i);
            Map<S, Double> nextBackward = backwardHistoryArrayList.get(i+1);
            O nextObservation = observations.get(i + 1);

            Map<S, Map<S, Double>> current = new HashMap<>();
            for (S state1 : states) {
                Map<S, Double> currentS = new HashMap<>();
                for (S state2 : states) {

                    double numerator = currentForward.get(state1) * transitionProbabilities.get(state1).get(state2);
                    numerator *= nextBackward.get(state2) * emissionProbabilities.get(state2).get(nextObservation);

                    double denominator = 0;

                    for (S k : states) {
                        for (S w : states) {
                            double partialDenominator = currentForward.get(k) * transitionProbabilities.get(k).get(w);
                            partialDenominator *= currentBackward.get(w) * emissionProbabilities.get(w).get(nextObservation);

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

        ArrayList<Map<S, Double>> forwardHistoryArrayList = new ArrayList<>(forwardHistory);
        ArrayList<Map<S, Double>> backwardHistoryArrayList = new ArrayList<>(backwardHistory);

        for (int i = 0; i < observations.size(); i++) {
            Map<S, Double> currentForward = forwardHistoryArrayList.get(i);
            Map<S, Double> currentBackward = backwardHistoryArrayList.get(i);

            Map<S, Double> current = new HashMap<>();
            for (S state1 : states) {

                double numerator = currentForward.get(state1) * currentBackward.get(state1);
                double denominator = 0;

                for (S state2 : states) {
                    denominator += currentForward.get(state2) * currentBackward.get(state2);
                }

                current.put(state1, numerator / denominator);
            }
            gammaHistory.add(current);
        }
        return gammaHistory;
    }

    public void baumWelch(List<O> observations, Map<S, Double> startProbabilities) {

        LinkedList<Double> scalingFactors = new LinkedList<>();

        System.out.println("\t FORWARD");
        List<Map<S, Double>> forwardHistory = forward(observations, scalingFactors, startProbabilities);
        System.out.println("\t BACKWARD");
        List<Map<S, Double>> backwardHistory = backward(observations, scalingFactors);

        System.out.println("\t GAMMA");
        List<Map<S, Double>> gammaHistory = gammaProbabilities(observations, forwardHistory, backwardHistory);
        System.out.println("\t XI");
        List<Map<S, Map<S, Double>>> xiHistory = xiProbabilities(observations, forwardHistory, backwardHistory);

        System.out.println("\t ACTUAL BAUMWELCH");
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
