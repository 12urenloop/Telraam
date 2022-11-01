package telraam.logic.baumwelch;

import io.dropwizard.jersey.setup.JerseyEnvironment;
import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.*;
import telraam.database.models.BatonSwitchover;
import telraam.database.models.Detection;
import telraam.database.models.Station;
import telraam.database.models.Team;
import telraam.logic.Lapper;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class BaumWelchLapper implements Lapper {

    private final Thread runner = new Thread(this::run);
    private final Jdbi jdbi;

    public BaumWelchLapper(Jdbi jdbi) {
        this.jdbi = jdbi;

        //run();
        runner.start();
    }

    private void run() {
        BatonDAO batonDAO = jdbi.onDemand(BatonDAO.class);
        BatonSwitchoverDAO batonSwitchoverDAO = jdbi.onDemand(BatonSwitchoverDAO.class);
        DetectionDAO detectionDAO = jdbi.onDemand(DetectionDAO.class);
        TeamDAO teamDAO = jdbi.onDemand(TeamDAO.class);
        StationDAO stationDAO = jdbi.onDemand(StationDAO.class);

        List<Team> teams = teamDAO.getAll();
        List<BatonSwitchover> batonSwitchovers = batonSwitchoverDAO.getAll().stream().sorted(Comparator.comparing(BatonSwitchover::getTimestamp)).toList();
        Map<Team, List<Detection>> teamToDetections = teams.stream().collect(Collectors.toMap(Function.identity(), (_t) -> new LinkedList<>()));

        Map<Integer, Team> teamIdToTeam = teams.stream().collect(Collectors.toMap(Team::getId, Function.identity()));

        int batonSwitchoverIndex = 0;
        Map<Integer, Team> batonIdToTeam = new HashMap<>();

        List<Detection> sortedDetections = detectionDAO.getAll().stream().sorted(Comparator.comparing(Detection::getTimestamp)).toList();
        for (Detection detection : sortedDetections) {
            while (batonSwitchoverIndex < batonSwitchovers.size() && batonSwitchovers.get(batonSwitchoverIndex).getTimestamp().before(detection.getTimestamp())) {
                BatonSwitchover switchover = batonSwitchovers.get(batonSwitchoverIndex);
                batonIdToTeam.put(switchover.getNewBatonId(), teamIdToTeam.get(switchover.getTeamId()));
                batonSwitchoverIndex += 1;
            }

            if (batonIdToTeam.containsKey(detection.getBatonId())) {
                List<Detection> detections = teamToDetections.get(batonIdToTeam.get(detection.getBatonId()));
                if (detections.size() > 0 && detections.get(detections.size() - 1).getTimestamp().getTime() == detection.getTimestamp().getTime()) {
                    if (detections.get(detections.size() - 1).getRssi() < detection.getRssi()) {
                        detections.set(detections.size() - 1, detection);
                    }
                } else {
                    detections.add(detection);
                }
            }
        }

        List<Station> stations = stationDAO.getAll();
        Map<Integer, Station> stationIdToStation = stations.stream().collect(Collectors.toMap(Station::getId, Function.identity()));

        for (Team team : teamToDetections.keySet()) {
            System.out.println("Running for team " + team.getName());
            List<Integer> observations = teamToDetections.get(team).stream().map(Detection::getStationId).toList();

            HMM<Integer, Integer> hmm = new HMM<>(IntStream.range(0, stations.size()).boxed().toList(), stations.stream().map(Station::getId).collect(Collectors.toList()));
            for (int i = 0; i < 20; i++) {
                hmm.baumWelch(observations, IntStream.range(0, stations.size()).boxed().collect(Collectors.toMap(Function.identity(), _station -> 1.0/stations.size())));
            }
            System.out.println("================================================================");
            System.out.println(hmm.getEmissionProbabilities());
            System.out.println(hmm.getTransitionProbabilities());
        }

        /*System.out.println(teamToDetections);

        HMM<Integer, Integer> hmm = new HMM<>(List.of(1, 2, 3, 4), List.of(1, 2, 3));
        System.out.println(hmm.getTransitionProbabilities());
        System.out.println(hmm.getEmissionProbabilities());

        var observations = List.of(1, 2, 3, 3, 3, 3, 3, 3, 3, 1);
        var forward = hmm.forward(observations, Map.of(1,0.25, 2,0.25, 3,0.25, 4,0.25));
        var backward = hmm.backward(observations);

        System.out.println(forward);
        System.out.println(backward);
        System.out.println(hmm.gammaProbabilities(observations, forward, backward));
        System.out.println(hmm.xiProbabilities(observations, forward, backward));

        System.out.println("==>");
        hmm.baumWelch(observations, Map.of(1,0.25, 2,0.25, 3,0.25, 4,0.25), 1);
        System.out.println(hmm.getTransitionProbabilities());
        System.out.println(hmm.getEmissionProbabilities());

        System.out.println("==>");
        hmm.baumWelch(observations, Map.of(1,0.25, 2,0.25, 3,0.25, 4,0.25), 1);
        System.out.println(hmm.getTransitionProbabilities());
        System.out.println(hmm.getEmissionProbabilities());

        System.out.println("==>");
        hmm.baumWelch(observations, Map.of(1,0.25, 2,0.25, 3,0.25, 4,0.25), 1);
        System.out.println(hmm.getTransitionProbabilities());
        System.out.println(hmm.getEmissionProbabilities());

        System.out.println("==>");
        hmm.baumWelch(observations, Map.of(1,0.25, 2,0.25, 3,0.25, 4,0.25), 1);
        System.out.println(hmm.getTransitionProbabilities());
        System.out.println(hmm.getEmissionProbabilities());

        System.out.println("==>");
        hmm.baumWelch(observations, Map.of(1,0.25, 2,0.25, 3,0.25, 4,0.25), 1);
        System.out.println(hmm.getTransitionProbabilities());
        System.out.println(hmm.getEmissionProbabilities());
        */
    }


    @Override
    public void handle(Detection msg) {
        if (!runner.isAlive()) {
            runner.start();
        }
    }

    @Override
    public void registerAPI(JerseyEnvironment jersey) {
    }
}
