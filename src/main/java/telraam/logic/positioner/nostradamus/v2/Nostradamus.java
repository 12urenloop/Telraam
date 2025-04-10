package telraam.logic.positioner.nostradamus.v2;

import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.BatonSwitchoverDAO;
import telraam.database.daos.PositionSourceDAO;
import telraam.database.daos.StationDAO;
import telraam.database.models.*;
import telraam.logic.positioner.Position;
import telraam.logic.positioner.PositionSender;
import telraam.logic.positioner.Positioner;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public class Nostradamus implements Positioner {
    private static final Logger logger = Logger.getLogger(Nostradamus.class.getName());
    private final String SOURCE_NAME = "nostradamus_v2";
    private final int MIN_RSSI = -90; // Minimum rssi strength that a detections needs to have
    private final int INTERVAL_FETCH_MS = 10000;
    private final int INTERVAL_UPDATE_MS = 200;
    private final int LENGTH_OFFSET = 10; // Distance from the last station to the finish in meter
    private final Jdbi jdbi;
    private final PositionSender positionSender;
    ConcurrentHashMap<Integer, ConcurrentLinkedQueue<Detection>> newDetections;
    private final Map<Integer, TeamHandler> teamHandlers;
    private final Map<Integer, StationData> stationData;

    public Nostradamus(Jdbi jdbi) {
        this.jdbi = jdbi;

        // Add as source
        PositionSourceDAO positionSourceDAO = jdbi.onDemand(PositionSourceDAO.class);
        if (positionSourceDAO.getByName(SOURCE_NAME).isEmpty()) {
            positionSourceDAO.insert(new PositionSource(SOURCE_NAME));
        }

        this.positionSender = new PositionSender(SOURCE_NAME);
        this.newDetections = new ConcurrentHashMap<>();
        this.teamHandlers = new ConcurrentHashMap<>();
        this.stationData = new HashMap<>();

        // Initialize station data list
        List<Station> stations = this.jdbi.onDemand(StationDAO.class).getAll();
        stations.sort(Comparator.comparing(Station::getDistanceFromStart));
        int length = (int) (stations.get(stations.size() - 1).getDistanceFromStart() + LENGTH_OFFSET);
        for (int i = 0; i < stations.size(); i++) {
            Station station = stations.get(i);
            int nextIdx = (i + 1) % stations.size();
            int distanceToNext = (int) ((stations.get(nextIdx).getDistanceFromStart() - station.getDistanceFromStart() + length ) % length);
            this.stationData.put(station.getId(), new StationData(
                    distanceToNext,
                    station.getDistanceFromStart() / length,
                    (double) distanceToNext / length,
                    stations.get(nextIdx).getId()));
        }

        new Thread(this::fetch).start();
        new Thread(this::update).start();
    }

    // Fetch updates team handlers based on switchovers
    private void fetch() {
        while (true) {
            List<BatonSwitchover> switchovers = jdbi.onDemand(BatonSwitchoverDAO.class).getAll();

            Map<Integer, Integer> batonToTeam = switchovers.stream().sorted(
                    Comparator.comparing(BatonSwitchover::getTimestamp)
            ).collect(Collectors.toMap(
                    BatonSwitchover::getNewBatonId,
                    BatonSwitchover::getTeamId,
                    (existing, replacement) -> replacement
            ));

            for (Map.Entry<Integer, Integer> entry: batonToTeam.entrySet()) {
                teamHandlers.compute(entry.getValue(), (teamId, existingTeam) -> {
                    if (existingTeam == null) {
                        return new TeamHandler(teamId, new AtomicInteger(entry.getKey()), stationData);
                    } else {
                        existingTeam.batonId.set(entry.getKey());
                        return existingTeam;
                    }
                });
            }

            try {
                Thread.sleep(INTERVAL_FETCH_MS);
            } catch (InterruptedException e) {
                logger.severe(e.getMessage());
            }
        }
    }

    // Update handles all new detections and sends new positions
    private void update() {
        List<Position> positions = new ArrayList<>();

        while (true) {
            positions.clear();

            for (TeamHandler team : teamHandlers.values()) {
                ConcurrentLinkedQueue<Detection> queue = newDetections.get(team.batonId.get());
                if (queue != null && !queue.isEmpty()) {
                    List<Detection> copy = new ArrayList<>();
                    Detection d;
                    while ((d = queue.poll()) != null) {
                        copy.add(d);
                    }

                    team.update(copy);
                }

                Position position = team.getPosition();
                if (position != null) {
                    positions.add(position);
                }
            }

            if (!positions.isEmpty()) {
                positionSender.send(positions);
            }

            try {
                Thread.sleep(INTERVAL_UPDATE_MS);
            } catch (InterruptedException e) {
                logger.severe(e.getMessage());
            }
        }
    }

    @Override
    public void handle(Detection detection) {
        if (detection.getRssi() > MIN_RSSI) {
            newDetections
                    .computeIfAbsent(detection.getBatonId(), k -> new ConcurrentLinkedQueue<>())
                    .add(detection);
        }
    }

}
