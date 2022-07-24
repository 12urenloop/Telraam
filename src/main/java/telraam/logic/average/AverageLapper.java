package telraam.logic.average;

import io.dropwizard.jersey.setup.JerseyEnvironment;
import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.*;
import telraam.database.models.*;
import telraam.logic.Lapper;

import java.util.*;

public class AverageLapper implements Lapper {

    private final Thread runner = new Thread(this::run);
    private Jdbi jdbi;

    public AverageLapper(Jdbi jdbi) {
        this.jdbi = jdbi;
        runner.start();
    }

    public synchronized void run() {
        DetectionDAO detectionDAO = jdbi.onDemand(DetectionDAO.class);
        StationDAO stationDAO = jdbi.onDemand(StationDAO.class);
        TeamDAO teamDAO = jdbi.onDemand(TeamDAO.class);
        BatonDAO batonDAO = jdbi.onDemand(BatonDAO.class);
        BatonSwitchoverDAO batonSwitchoverDAO = jdbi.onDemand(BatonSwitchoverDAO.class);

        List<Baton> batons = batonDAO.getAll();
        List<BatonSwitchover> batonSwitchovers = batonSwitchoverDAO.getAll();

        List<Station> stations = stationDAO.getAll();
        List<Team> teams = teamDAO.getAll();
        HashMap<Integer, HashMap<Integer, LinkedList<DetectionInterval>>> detectionIntervalMap = new HashMap<>();
        for (Station station : stations) {
            HashMap<Integer, LinkedList<DetectionInterval>> teamDetectionIntervalMap = new HashMap<>();
            for (Team team : teams) {
                teamDetectionIntervalMap.put(team.getId(), new LinkedList<>());
            }
            detectionIntervalMap.put(station.getId(), teamDetectionIntervalMap);
        }

        int switchoverIndex = 0;
        Map<Integer, Integer> batonIdToTeamId = new HashMap<>();


        List<Detection> detections = detectionDAO.getAll();
        //detections.removeIf((detection) -> detection.getRssi() < -80);


        for (Detection detection : detections) {
            while (switchoverIndex < batonSwitchovers.size() && batonSwitchovers.get(switchoverIndex).getTimestamp().before(detection.getTimestamp())) {
                BatonSwitchover switchover = batonSwitchovers.get(switchoverIndex);
                batonIdToTeamId.put(switchover.getNewBatonId(), switchover.getTeamId());
                switchoverIndex += 1;
            }

            if (!batonIdToTeamId.containsKey(detection.getBatonId())) {
                continue;
            }

            HashMap<Integer, LinkedList<DetectionInterval>> teamDetectionIntervalMap = detectionIntervalMap.get(detection.getStationId());
            LinkedList<DetectionInterval> detectionIntervals = teamDetectionIntervalMap.get(batonIdToTeamId.get(detection.getBatonId()));
            DetectionInterval lastDetectionInterval = detectionIntervals.peekLast();
            if (lastDetectionInterval == null) {
                detectionIntervals.add(new DetectionInterval(detection.getTimestamp()));
            } else {
                long lastDetectionIntervalTime = lastDetectionInterval.getEndTime().getTime();
                if (detection.getTimestamp().getTime() <= lastDetectionIntervalTime + (20*1000)) {
                    //Detection is in 15s interval.
                    lastDetectionInterval.setEndTime(detection.getTimestamp());
                } else {
                    detectionIntervals.add(new DetectionInterval(detection.getTimestamp()));
                }
            }
        }

        for (Integer stationId : detectionIntervalMap.keySet()) {
            System.out.println("Station " + stationId);
            for (Integer teamId : detectionIntervalMap.get(stationId).keySet()) {
                System.out.println("\tTeam " + teamId + " counted " + detectionIntervalMap.get(stationId).get(teamId).stream().filter(interval -> interval.getCount() >= 8).count() + " laps.");
            }
        }

        try {
            Thread.sleep(10_000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    @Override
    public synchronized void handle(Detection msg) {
        if (!runner.isAlive()) {
            runner.start();
        }
    }

    @Override
    public void registerAPI(JerseyEnvironment jersey) {

    }
}
