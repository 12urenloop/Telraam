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
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class ViterbiLapper implements Lapper {
    static final String SOURCE_NAME = "viterbi-lapper";

    private final ViterbiLapperConfiguration config;
    private final ViterbiModel<Integer, Integer> viterbiModel;
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

        BeaconDAO beaconDAO = jdbi.onDemand(BeaconDAO.class);
        LapSourceDAO lapSourceDAO = jdbi.onDemand(LapSourceDAO.class);

        lapSourceDAO.getByName(ViterbiLapper.SOURCE_NAME).orElseThrow();

        this.lapSourceId = lapSourceDAO.getByName(ViterbiLapper.SOURCE_NAME).get().getId();

        Set<Integer> observations = beaconDAO.getAll().stream().map(Beacon::getId).collect(Collectors.toSet());
        Set<Integer> hiddenStates = IntStream.range(0, this.config.SECTOR_STARTS.length).boxed().collect(Collectors.toSet());

        this.viterbiModel = new ViterbiModel<>(
                observations,
                hiddenStates,
                calculateTransitionProbabilities(hiddenStates),
                calculateEmissionProbabilities(beaconDAO.getAll()),
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

    private Map<Integer, Map<Integer, Double>> calculateEmissionProbabilities(List<Beacon> stations) {
        stations.sort(Comparator.comparing(Beacon::getDistanceFromStart));
        Map<Integer, Map<Integer, Double>> ret = new HashMap<>();
        for (int i = 0; i < stations.size(); i++) {
            Map<Integer, Double> m = new HashMap<>();
            for (int j = 0; j < stations.size(); j++) {
                m.put(stations.get(j).getId(), 0.0);
            }
            m.put(stations.get(i).getId(), 1.0);
            ret.put(i, m);
        }

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
        for (int i: hiddenStates) {
            Map<Integer, Double> probabilities = new HashMap<>();
            for (int j : hiddenStates) {
                if (i == j || j == (i+1)%hiddenStates.size()) {
                    probabilities.put(j, 0.5);
                } else {
                    probabilities.put(j, 0.0);
                }
            }
            transitionProbabilities.put(i, probabilities);
        }

        return transitionProbabilities;
    }

    @Override
    public synchronized void handle(Detection msg) {
        if (!this.debounceScheduled) {
            // TODO: this might be better as an atomic
            this.debounceScheduled = true;
            this.scheduler.schedule(() -> {
                try {
                    this.calculateLaps();
                } catch ( Exception e ) {
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

        Map<Integer, ViterbiAlgorithm<Integer>> viterbis = teams.stream()
                .collect(Collectors.toMap(Team::getId, _team -> new ViterbiAlgorithm<>(this.viterbiModel)));

        Map<Integer, Integer> batonIdToTeamId = teams.stream()
                .collect(Collectors.toMap(Team::getBatonId, Team::getId));

        // BREAKING TODO: We need a way to find the team a baton was assigned to at the time of detection
        // This should probably be tagged at detection ingestion time
        for (Detection detection : detections) {
            int teamId = batonIdToTeamId.get(detection.getBatonId());
            viterbis.get(teamId).observe(detection.getBeaconId());
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
        return this.viterbiModel;
    }
}
