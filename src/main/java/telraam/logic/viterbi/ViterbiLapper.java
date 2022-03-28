package telraam.logic.viterbi;

import io.dropwizard.jersey.setup.JerseyEnvironment;
import io.swagger.models.auth.In;
import org.apache.commons.math3.distribution.NormalDistribution;
import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.*;
import telraam.database.models.*;
import telraam.logic.Lapper;
import telraam.logic.viterbi.algorithm.ViterbiAlgorithm;
import telraam.logic.viterbi.algorithm.ViterbiModel;
import telraam.logic.viterbi.algorithm.ViterbiState;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class ViterbiLapper implements Lapper {
    static final String SOURCE_NAME = "viterbi-lapper";

    private final ViterbiLapperConfiguration config;
    private Map<Integer, ViterbiState> currentStates;
    private final Jdbi jdbi;
    private final int lapSourceId;
    private final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);
    private boolean debounceScheduled;

    public ViterbiLapper(Jdbi jdbi) {
        this(jdbi, new ViterbiLapperConfiguration());
    }

    public ViterbiLapper(Jdbi jdbi, ViterbiLapperConfiguration configuration) {
        this.jdbi = jdbi;
        this.config = configuration;
        this.currentStates = new HashMap<>();
        this.debounceScheduled = false;

        LapSourceDAO lapSourceDAO = jdbi.onDemand(LapSourceDAO.class);

        lapSourceDAO.getByName(ViterbiLapper.SOURCE_NAME).orElseThrow();

        this.lapSourceId = lapSourceDAO.getByName(ViterbiLapper.SOURCE_NAME).get().getId();
    }


    private ViterbiModel createViterbiModel() {
        BeaconDAO beaconDAO = jdbi.onDemand(BeaconDAO.class);

        // We will construct one segment for each beacon, which will represent its
        // neighbourhood.
        List<Beacon> beacons = beaconDAO.getAll();
        beacons.sort(Comparator.comparing(Beacon::getDistanceFromStart));


        Map<Integer, Map<Integer, Double>> emissionProbabilities = new HashMap<>();
        for (int segmentNum = 0; segmentNum < beacons.size(); segmentNum++) {
            Map<Integer, Double> probas = new HashMap<>();
            for (int beaconNum = 0; beaconNum < beacons.size(); beaconNum++) {
                int beaconId = beacons.get(beaconNum).getId();
                if (segmentNum == beaconNum) {
                    probas.put(beaconId, this.config.SAME_STATION_DETECTION_CHANCE);
                } else {
                    probas.put(beaconId, this.config.DIFFERENT_STATION_DETECTION_CHANCE);
                }
            }
            emissionProbabilities.put(segmentNum, probas);
        }

        Map<Integer, Map<Integer, Double>> transitionProbabilities = new HashMap<>();
        for (int prevSegment = 0; prevSegment < beacons.size(); prevSegment++) {
            Map<Integer, Double> probas = new HashMap<>();
            double sum = 0.0;

            // a station is skipped if all detections are missed
            double skipStationProbability = Math.pow(1 - this.config.SAME_STATION_DETECTION_CHANCE, this.config.EXPECTED_NUM_DETECTIONS);

            // calculate numbers this way so that backwards steps are rounded down
            // and forward steps is rounded up
            int numStepsBackwards = (beacons.size() - 1) / 2;
            int numStepsForwards = beacons.size() - 1 - numStepsBackwards;

            double sameStationWeight = this.config.SAME_STATION_DETECTION_CHANCE * this.config.EXPECTED_NUM_DETECTIONS;
            // add 2: one unit of weigth for running forwards, one for running backwards
            probas.put(prevSegment, sameStationWeight / (sameStationWeight + 2));

            // transition probabilities for running forwards
            // curBaseProba is the probability mass that should still be distributed
            double curBaseProba = 1 / (sameStationWeight + 2);
            for (int i = 1; i <= numStepsForwards; i++) {
                // compute next segment index
                int nextSegment = Math.floorMod(prevSegment + i, beacons.size());
                double proba = curBaseProba;
                if (i < numStepsForwards) {
                    // multiply by the probability that this station was not skipped.
                    // When this is the final step, we do not consider the possibility of skipping anymore
                    // (so that probabilities add up to 1)
                    proba *= (1 - skipStationProbability);
                }
                probas.put(nextSegment, proba);

                // subtract the used amount of probability mass
                curBaseProba -= proba;
            }

            // transition probabilities for running backwards
            // refer to above comments
            curBaseProba = 1 / (sameStationWeight + 2);
            for (int i = 1; i <= numStepsBackwards; i++) {
                int nextSegment = Math.floorMod(prevSegment - i, beacons.size());
                double proba = curBaseProba;
                if (i < numStepsBackwards) {
                    proba *= (1 - skipStationProbability);
                }
                probas.put(nextSegment, proba);
                curBaseProba -= proba;
            }

            transitionProbabilities.put(prevSegment, probas);
        }

        return new ViterbiModel<>(
                beacons.stream().map(Beacon::getId).collect(Collectors.toSet()),
                IntStream.range(0, beacons.size()).boxed().collect(Collectors.toSet()),
                transitionProbabilities,
                emissionProbabilities,
                calculateStartProbabilities()
        );
    }

    public Map<Integer, double[]> getProbabilities() {
        Map<Integer, double[]> ret = new HashMap<>();
        for (Map.Entry<Integer, ViterbiState> entry : this.currentStates.entrySet()) {
            ret.put(entry.getKey(), entry.getValue().probabilities());
        }
        return ret;
    }

    public Map<Integer, int[]> getLapCounts() {
        Map<Integer, int[]> ret = new HashMap<>();
        for (Map.Entry<Integer, ViterbiState> entry : this.currentStates.entrySet()) {
            ret.put(entry.getKey(), entry.getValue().lapCounts());
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

    @Override
    public synchronized void handle(Detection msg) {
        if (!this.debounceScheduled) {
            // TODO: this might be better as an atomic
            this.debounceScheduled = true;
            this.scheduler.schedule(() -> {
                try {
                    this.calculateLaps();
                } catch (Exception e) {
                    System.err.println("Something went wrong");
                    e.printStackTrace();
                }
                this.debounceScheduled = false;
            }, this.config.DEBOUNCE_TIMEOUT, TimeUnit.SECONDS);
        }
    }

    private synchronized void calculateLaps() {
        System.out.println("Calculating laps");
        // TODO: this implementation does not take lap timestamps into account

        TeamDAO teamDAO = this.jdbi.onDemand(TeamDAO.class);
        DetectionDAO detectionDAO = this.jdbi.onDemand(DetectionDAO.class);
        LapDAO lapDAO = this.jdbi.onDemand(LapDAO.class);
        List<Team> teams = teamDAO.getAll();

        // TODO: stream these from the database
        List<Detection> detections = detectionDAO.getAll();
        detections.sort(Comparator.comparing(Detection::getTimestamp));

        // we create a viterbi model each time because the set of stations is not static
        ViterbiModel<Integer, Integer> viterbiModel = createViterbiModel();

        Map<Integer, ViterbiAlgorithm<Integer>> viterbis = teams.stream()
                .collect(Collectors.toMap(Team::getId, _team -> new ViterbiAlgorithm<>(viterbiModel)));

        Map<Integer, Integer> batonIdToTeamId = teams.stream()
                .collect(Collectors.toMap(Team::getBatonId, Team::getId));

        // BREAKING TODO: We need a way to find the team a baton was assigned to at the time of detection
        // This should probably be tagged at detection ingestion time
        for (Detection detection : detections) {
            if (batonIdToTeamId.containsKey(detection.getBatonId())) {
                int teamId = batonIdToTeamId.get(detection.getBatonId());
                viterbis.get(teamId).observe(detection.getBeaconId());
            }
        }

        this.currentStates = viterbis.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, entry -> entry.getValue().getState()));

        // We made a new estimation of the lap count, now we can update the database state to match

        Map<Integer, TreeSet<Lap>> lapsByTeam = teams.stream().collect(Collectors.toMap(Team::getId, x -> new TreeSet<>(Comparator.comparing(Lap::getTimestamp))));

        for (Lap lap : lapDAO.getAllBySource(this.lapSourceId)) {
            lapsByTeam.get(lap.getTeamId()).add(lap);
        }

        for (Map.Entry<Integer, ViterbiState> entry : this.currentStates.entrySet()) {
            int teamId = entry.getKey();
            TreeSet<Lap> laps = lapsByTeam.get(teamId);

            long previousLapCount = laps.size();
            ViterbiState state = entry.getValue();
            long newLapCount = state.lapCounts()[state.mostLikelySegment()];

            // add laps that were not counted yet
            for (long lapCount = previousLapCount; lapCount < newLapCount; lapCount++) {
                lapDAO.insert(new Lap(teamId, this.lapSourceId, Timestamp.from(Instant.now())));
            }
            // remove laps that were an overestimation
            for (long lapCount = previousLapCount; lapCount > newLapCount; lapCount--) {
                Lap lapToRemove = laps.last();
                laps.remove(lapToRemove);
                lapDAO.deleteById(lapToRemove.getId());
            }
        }
    }

    @Override
    public void registerAPI(JerseyEnvironment jersey) {
        jersey.register(new ViterbiLapperResource(this));
    }

    public ViterbiLapperConfiguration getConfig() {
        return this.config;
    }

    public ViterbiModel<Integer, Integer> getModel() {
        return this.createViterbiModel();
    }
}
