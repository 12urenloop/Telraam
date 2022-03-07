package telraam.logic.viterbi;

import io.dropwizard.jersey.setup.JerseyEnvironment;
import org.apache.commons.math3.distribution.NormalDistribution;
import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.BatonDAO;
import telraam.database.daos.BeaconDAO;
import telraam.database.models.Baton;
import telraam.database.models.Beacon;
import telraam.database.models.Detection;
import telraam.logic.Lapper;
import telraam.logic.viterbi.algorithm.ViterbiAlgorithm;
import telraam.logic.viterbi.algorithm.ViterbiModel;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class ViterbiLapper implements Lapper {
    static final String SOURCE_NAME = "viterbi-lapper";

    private final Map<Integer, ViterbiAlgorithm<Integer, Integer>> viterbis;
    private final ViterbiLapperConfiguration config;
    private final ViterbiModel<Integer, Integer> viterbiModel;

    public ViterbiLapper(Jdbi jdbi) {
        this(jdbi, new ViterbiLapperConfiguration());
    }

    public ViterbiLapper(Jdbi jdbi, ViterbiLapperConfiguration configuration) {
        this.viterbis = new HashMap<>();
        this.config = configuration;

        BatonDAO batonDAO = jdbi.onDemand(BatonDAO.class);
        BeaconDAO beaconDAO = jdbi.onDemand(BeaconDAO.class);

        Set<Integer> observations = beaconDAO.getAll().stream().map(Beacon::getId).collect(Collectors.toSet());
        Set<Integer> hiddenStates = IntStream.range(0, this.config.SECTOR_STARTS.length).boxed().collect(Collectors.toSet());

        this.viterbiModel = new ViterbiModel<>(
                observations,
                hiddenStates,
                calculateTransitionProbabilities(hiddenStates),
                calculateEmissionProbabilities(beaconDAO.getAll()),
                calculateStartProbabilities()
        );


        for (Baton baton : batonDAO.getAll()) {
            ViterbiAlgorithm<Integer> viterbi = new ViterbiAlgorithm<>(this.viterbiModel);
            this.viterbis.put(baton.getId(), viterbi);
        }
    }

    public Map<Integer, double[]> getProbabilities() {
        Map<Integer, double[]> ret = new HashMap<>();
        for (Map.Entry<Integer, ViterbiAlgorithm<Integer>> entry : this.viterbis.entrySet()) {
            ret.put(entry.getKey(), entry.getValue().getState().getProbabilities());
        }
        return ret;
    }

    private Map<Integer, Double> calculateStartProbabilities() {
        Map<Integer, Double> ret = new HashMap<>();

        ret.put(0, 1.0 - (this.config.SECTOR_STARTS.length - 1) * this.config.RESTART_PROBABILITY);
        for (int i = 1; i < this.config.SECTOR_STARTS.length; i++) {
            ret.put(i, this.config.RESTART_PROBABILITY);
        }

        return ret;
    }

    private Map<Integer, Map<Integer, Double>> calculateEmissionProbabilities(List<Beacon> stations) {
        Map<Integer, Map<Integer, Double>> ret = new HashMap<>();
        for (int sectorIndex = 0; sectorIndex < this.config.SECTOR_STARTS.length - 1; sectorIndex++) {
            ret.put(sectorIndex, calculateSectorProbabilities(this.config.SECTOR_STARTS[sectorIndex], this.config.SECTOR_STARTS[sectorIndex+1], stations));
        }
        ret.put(this.config.SECTOR_STARTS.length-1, calculateSectorProbabilities(this.config.SECTOR_STARTS[this.config.SECTOR_STARTS.length-1], this.config.TRACK_LENGTH, stations));

        return ret;
    }

    private Map<Integer, Double> calculateSectorProbabilities(int start, int end, List<Beacon> stations) {
        Map<Integer, Double> sectorProbabilities = new HashMap<>();
        for (Beacon station : stations) {
            double probability = 0.0;

            // Detecting next lap
            NormalDistribution stationDistribution = new NormalDistribution(station.getDistanceFromStart() - this.config.TRACK_LENGTH, this.config.STATION_RANGE_SIGMA);
            probability += stationDistribution.cumulativeProbability(start, end);

            // Detecting current lap
            stationDistribution = new NormalDistribution(station.getDistanceFromStart(), this.config.STATION_RANGE_SIGMA);
            probability += stationDistribution.cumulativeProbability(start, end);

            // Detecting previous lap
            stationDistribution = new NormalDistribution(station.getDistanceFromStart() + this.config.TRACK_LENGTH, this.config.STATION_RANGE_SIGMA);
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
        for (int i = 0; i < this.config.SECTOR_STARTS.length - 1; i++) {
            double expectedDetections = ((this.config.SECTOR_STARTS[i+1] - this.config.SECTOR_STARTS[i]) / this.config.AVERAGE_RUNNER_SPEED) * this.config.DETECTIONS_PER_SECOND;
            transitionProbabilities.get(i).put(i, expectedDetections / (expectedDetections + 1));
            transitionProbabilities.get(i).put(i + 1, 1 / (expectedDetections + 1));
        }

        double expectedDetections = ((this.config.TRACK_LENGTH - this.config.SECTOR_STARTS[this.config.SECTOR_STARTS.length - 1]) / this.config.AVERAGE_RUNNER_SPEED) * this.config.DETECTIONS_PER_SECOND;
        transitionProbabilities.get(this.config.SECTOR_STARTS.length - 1).put(this.config.SECTOR_STARTS.length - 1, expectedDetections / (expectedDetections + 1));
        transitionProbabilities.get(this.config.SECTOR_STARTS.length - 1).put(0, 1 / (expectedDetections + 1));

        return transitionProbabilities;
    }

    @Override
    public void handle(Detection msg) {
        ViterbiAlgorithm<Integer> viterbiAlgorithm = this.viterbis.get(msg.getBatonId());
        viterbiAlgorithm.observe(msg.getBeaconId());
        System.out.println("Baton " + msg.getBatonId() + " is now probably at " + viterbiAlgorithm.getState().mostLikelyState());
    }

    @Override
    public void registerAPI(JerseyEnvironment jersey) {
        jersey.register(new ViterbiLapperResource(this));
    }

    public ViterbiLapperConfiguration getConfig() {
        return this.config;
    }

    public ViterbiModel<Integer, Integer> getModel() {
        return this.viterbiModel;
    }
}
