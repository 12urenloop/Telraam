package telraam.logic.viterbi;

import io.dropwizard.jersey.setup.JerseyEnvironment;
import io.swagger.models.auth.In;
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
import java.util.concurrent.TimeUnit;
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


    private ViterbiModel<Integer, Integer> createViterbiModel() {
        StationDAO stationDAO = jdbi.onDemand(StationDAO.class);

        // We will construct one segment for each station, which will represent its
        // neighbourhood.
        List<Station> stations = stationDAO.getAll();
        stations.sort(Comparator.comparing(Station::getDistanceFromStart));


        // ***********************************
        // Build detection probability mapping
        // ***********************************
        Map<Integer, Map<Integer, Double>> emissionProbabilities = new HashMap<>();
        for (int segmentNum = 0; segmentNum < stations.size(); segmentNum++) {
            Map<Integer, Double> probas = new HashMap<>();
            for (int stationNum = 0; stationNum < stations.size(); stationNum++) {
                int stationId = stations.get(stationNum).getId();
                if (segmentNum == stationNum) {
                    probas.put(stationId, this.config.SAME_STATION_DETECTION_CHANCE);
                } else {
                    probas.put(stationId, this.config.BASE_DETECTION_CHANCE);
                }
            }
            emissionProbabilities.put(segmentNum, probas);
        }

        // ************************************
        // Build transition probability mapping
        // ************************************
        Map<Integer, Map<Integer, Double>> transitionProbabilities = new HashMap<>();
        for (int prevSegment = 0; prevSegment < stations.size(); prevSegment++) {
            Map<Integer, Double> probas = new HashMap<>();

            // a station is skipped when it is broken, or when all detections are missed
            // TODO: this currently does not take into account detections by other stations
            double skipStationProbability = this.config.BROKEN_STATION_PROBABILITY + Math.pow(
                    1 - this.config.SAME_STATION_DETECTION_CHANCE,
                    this.config.EXPECTED_NUM_DETECTIONS
            );

            // calculate amount of steps this way so that backwards steps are rounded down
            // and forward steps is rounded up (because we'd like to assume people run in the right direction)
            int numStepsBackwards = (stations.size() - 1) / 2;
            int numStepsForwards = stations.size() - 1 - numStepsBackwards;

            // This represents the odds (sameStationWeight against one) of staying on the same station
            // "how much more likely is it to stay, compared to moving"
            double sameStationWeight = (1 - this.config.BROKEN_STATION_PROBABILITY) * this.config.EXPECTED_NUM_DETECTIONS;
            // add 2: one unit of weight for running forwards, one for running backwards
            probas.put(prevSegment, sameStationWeight / (sameStationWeight + 2));

            // transition probabilities for running forwards.
            // To be precise: these probabilities should represent the probability that you will be at nextSegment
            // when the next detection happens. So assuming that you cannot be detected when you are in segment 2,
            // the transition probabilities towards segment 2 should always be 0.
            // This is because the viterbi algorithm runs in discrete time-steps, where each step corresponds
            // to a single detection (and is only vaguely related to the passage of time or distance).
            double remainingProbabilityMass = 1 / (sameStationWeight + 2);
            for (int i = 1; i <= numStepsForwards; i++) {
                // compute next segment index
                int nextSegment = Math.floorMod(prevSegment + i, stations.size());
                double proba = remainingProbabilityMass;
                if (i < numStepsForwards) {
                    // multiply by the probability that this station was not skipped.
                    // When this is the final step, we do not consider the possibility of skipping anymore
                    // (so that probabilities add up to 1)
                    proba *= (1 - skipStationProbability);
                }
                probas.put(nextSegment, proba);

                // subtract the used amount of probability mass
                remainingProbabilityMass -= proba;
            }

            // transition probabilities for running backwards
            // refer to above comments
            remainingProbabilityMass = 1 / (sameStationWeight + 2);
            for (int i = 1; i <= numStepsBackwards; i++) {
                int nextSegment = Math.floorMod(prevSegment - i, stations.size());
                double proba = remainingProbabilityMass;
                if (i < numStepsBackwards) {
                    proba *= (1 - skipStationProbability);
                }
                probas.put(nextSegment, proba);
                remainingProbabilityMass -= proba;
            }

            transitionProbabilities.put(prevSegment, probas);
        }

        return new ViterbiModel<>(
                stations.stream().map(Station::getId).collect(Collectors.toSet()),
                IntStream.range(0, stations.size()).boxed().collect(Collectors.toSet()),
                transitionProbabilities,
                emissionProbabilities,
                calculateStartProbabilities()
        );
    }

    public Map<Integer, Map<Integer, Double>> getProbabilities() {
        Map<Integer, Map<Integer, Double>> ret = new HashMap<>();
        for (Map.Entry<Integer, ViterbiState> entry : this.currentStates.entrySet()) {
            ret.put(entry.getKey(), entry.getValue().probabilities());
        }
        return ret;
    }

    public Map<Integer, Map<Integer, Integer>> getLapCounts() {
        Map<Integer, Map<Integer, Integer>> ret = new HashMap<>();
        for (Map.Entry<Integer, ViterbiState> entry : this.currentStates.entrySet()) {
            ret.put(entry.getKey(), entry.getValue().lapCounts());
        }
        return ret;
    }

    private Map<Integer, Double> calculateStartProbabilities() {
        StationDAO stationDAO = jdbi.onDemand(StationDAO.class);
        List<Station> stations = stationDAO.getAll();
        int numStations = stations.size();

        Map<Integer, Double> ret = new HashMap<>();

        ret.put(0, 1.0 - (numStations - 1) * this.config.RESTART_PROBABILITY);
        for (int i = 1; i < numStations; i++) {
            ret.put(i, this.config.RESTART_PROBABILITY);
        }

        return ret;
    }

    @Override
    public synchronized void handle(Detection msg) {
        if (msg.getRssi() < -70) {
            return;
        }
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
        BatonSwitchoverDAO batonSwitchoverDAO = this.jdbi.onDemand(BatonSwitchoverDAO.class);
        List<Team> teams = teamDAO.getAll();
        List<BatonSwitchover> switchovers = batonSwitchoverDAO.getAll();

        // TODO: stream these from the database
        List<Detection> detections = detectionDAO.getAll();
        detections.removeIf((detection) -> detection.getRssi() < -70);
        detections.sort(Comparator.comparing(Detection::getTimestamp));

        // we create a viterbi model each time because the set of stations is not static
        ViterbiModel<Integer, Integer> viterbiModel = createViterbiModel();

        Map<Integer, ViterbiAlgorithm<Integer>> viterbis = teams.stream()
                .collect(Collectors.toMap(Team::getId, _team -> new ViterbiAlgorithm<>(viterbiModel)));

        Map<Integer, Integer> batonIdToTeamId = new HashMap<>();

        int switchoverIndex = 0;

        for (Detection detection : detections) {
            while (switchoverIndex < switchovers.size() && switchovers.get(switchoverIndex).getTimestamp().before(detection.getTimestamp())) {
                BatonSwitchover switchover = switchovers.get(switchoverIndex);
                batonIdToTeamId.put(switchover.getNewBatonId(), switchover.getTeamId());
                switchoverIndex += 1;
            }

            if (batonIdToTeamId.containsKey(detection.getBatonId())) {
                int teamId = batonIdToTeamId.get(detection.getBatonId());
                viterbis.get(teamId).observe(detection.getStationId());
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
            long newLapCount = state.lapCounts().get(state.mostLikelySegment());

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

        System.out.println("Done calculating laps");
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
