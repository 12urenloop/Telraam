package telraam.logic;

import org.apache.commons.math3.distribution.NormalDistribution;
import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.BatonDAO;
import telraam.database.daos.BeaconDAO;
import telraam.database.models.Baton;
import telraam.database.models.Beacon;
import telraam.database.models.Detection;
import telraam.logic.viterbi.ViterbiAlgorithm;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class ViterbiLapper implements Lapper {
    static final String SOURCE_NAME = "viterbi-lapper";

    // ----- CONFIGURATION PARAMETERS -----
    private static final int TRACK_LENGTH = 500; // In meters
    private static final int[] SECTOR_STARTS = {0, 100, 150, 250, 350}; // In meters, the final sector ends at TRACK_LENGTH
    private static final double AVERAGE_RUNNER_SPEED = 3; // In meters per second
    private static final double DETECTIONS_PER_SECOND = 8; // The number of detections per station, per second
    private static final double STATION_RANGE_SIGMA = 50; // The sigma parameter of the detection probability of the stations
    private static final double RESTART_PROBABILITY = 0.001; // The probability that the runners wil start the race in a different spot than the start/finish line (should only happen on complete restarts)

    private final Map<Integer, ViterbiAlgorithm<Integer, Integer>> viterbis;

    public ViterbiLapper(Jdbi jdbi) {
        this.viterbis = new HashMap<>();

        BatonDAO batonDAO = jdbi.onDemand(BatonDAO.class);
        BeaconDAO beaconDAO = jdbi.onDemand(BeaconDAO.class);

        Set<Integer> observations = beaconDAO.getAll().stream().map(Beacon::getId).collect(Collectors.toSet());
        Set<Integer> hiddenStates = IntStream.range(0, SECTOR_STARTS.length).boxed().collect(Collectors.toSet());

        for (Baton baton : batonDAO.getAll()) {
            ViterbiAlgorithm<Integer, Integer> viterbi = new ViterbiAlgorithm<>(
                observations,
                hiddenStates,
                calculateTransitionProbabilities(hiddenStates),
                calculateEmissionProbabilities(beaconDAO.getAll()),
                calculateStartProbabilities()
            );
            this.viterbis.put(baton.getId(), viterbi);
        }
    }

    private Map<Integer, Double> calculateStartProbabilities() {
        Map<Integer, Double> ret = new HashMap<>();

        ret.put(0, 1.0 - (SECTOR_STARTS.length - 1) * RESTART_PROBABILITY);
        for (int i = 1; i < SECTOR_STARTS.length; i++) {
            ret.put(i, RESTART_PROBABILITY);
        }

        return ret;
    }

    private Map<Integer, Map<Integer, Double>> calculateEmissionProbabilities(List<Beacon> stations) {
        Map<Integer, Map<Integer, Double>> ret = new HashMap<>();
        for (int sectorIndex = 0; sectorIndex < SECTOR_STARTS.length - 1; sectorIndex++) {
            ret.put(sectorIndex, calculateSectorProbabilities(SECTOR_STARTS[sectorIndex], SECTOR_STARTS[sectorIndex+1], stations));
        }
        ret.put(SECTOR_STARTS.length, calculateSectorProbabilities(SECTOR_STARTS[SECTOR_STARTS.length-1], TRACK_LENGTH, stations));

        return ret;
    }

    private Map<Integer, Double> calculateSectorProbabilities(int start, int end, List<Beacon> stations) {
        Map<Integer, Double> sectorProbabilities = new HashMap<>();
        for (Beacon station : stations) {
            double probability = 0.0;

            // Detecting next lap
            NormalDistribution stationDistribution = new NormalDistribution(station.getDistanceFromStart(), STATION_RANGE_SIGMA - TRACK_LENGTH);
            probability += stationDistribution.cumulativeProbability(start, end);

            // Detecting current lap
            stationDistribution = new NormalDistribution(station.getDistanceFromStart(), STATION_RANGE_SIGMA);
            probability += stationDistribution.cumulativeProbability(start, end);

            // Detecting previous lap
            stationDistribution = new NormalDistribution(station.getDistanceFromStart(), STATION_RANGE_SIGMA + TRACK_LENGTH);
            probability += stationDistribution.cumulativeProbability(start, end);

            sectorProbabilities.put(station.getId(), probability);
        }
        return sectorProbabilities;
    }


    private Map<Integer, Map<Integer, Double>> calculateTransitionProbabilities(Set<Integer> hiddenStates) {
        Map<Integer, Map<Integer, Double>> transitionProbabilities = new HashMap<>();
        for (int i : hiddenStates) {
            Map<Integer, Double> probabilities = new HashMap<>();

            for (int j : hiddenStates) {
                probabilities.put(j, 0.0); // Initialize all probabilities to 0
            }

            transitionProbabilities.put(i, probabilities);
        }
        for (int i = 0; i < SECTOR_STARTS.length - 1; i++) {
            double expectedDetections = ((SECTOR_STARTS[i+1] - SECTOR_STARTS[i]) / AVERAGE_RUNNER_SPEED) * DETECTIONS_PER_SECOND;
            transitionProbabilities.get(i).put(i, expectedDetections / (expectedDetections + 1));
            transitionProbabilities.get(i).put(i + 1, 1 / (expectedDetections + 1));
        }

        double expectedDetections = ((TRACK_LENGTH - SECTOR_STARTS[SECTOR_STARTS.length - 1]) / AVERAGE_RUNNER_SPEED) * DETECTIONS_PER_SECOND;
        transitionProbabilities.get(SECTOR_STARTS.length - 1).put(SECTOR_STARTS.length - 1, expectedDetections / (expectedDetections + 1));
        transitionProbabilities.get(SECTOR_STARTS.length - 1).put(0, 1 / (expectedDetections + 1));

        return transitionProbabilities;
    }

    @Override
    public void handle(Detection msg) {
        this.viterbis.get(msg.getBatonId()).observe(msg.getBeaconId());
    }
}
