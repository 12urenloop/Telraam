package telraam.logic.positioner.nostradamus.v3;

import telraam.database.models.Detection;
import telraam.logic.positioner.Position;

import java.sql.Timestamp;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

public class TeamHandler {
    private static final Logger logger = Logger.getLogger(TeamHandler.class.getName());
    private final double AVG_SPEED = 0.006; // Average sprinting speed (m / ms), results in a lap of 55 seconds in the 12ul
    private final int INTERVAL = 2000; // Only keep detections in a x ms interval
    private final int MAX_TIMES = 21; // Amount of speeds to keep track of to determine the median
    private  final int teamId;
    public AtomicInteger batonId;
    private final Map<Integer, StationData> stationDataMap; // Map from station id to StationData
    private final Map<Integer, List<Double>> stationSpeeds; // Avg speed (progress / ms) to go from a stationId to the next
    private int currentStation; // Current station id
    private final List<Position> positions;
    private final LinkedList<Detection> detections;
    private Detection currentStationDetection;

    public TeamHandler(int teamId, AtomicInteger batonId, Map<Integer, StationData> stationDataMap) {
        this.teamId = teamId;
        this.batonId = batonId;
        this.stationDataMap = stationDataMap;

        this.stationSpeeds = new HashMap<>();
        this.positions = new ArrayList<>();
        this.detections = new LinkedList<>();

        this.currentStation = -1;
        this.positions.add(new Position(teamId, 0, 0, 0, 0));
        this.detections.add(new Detection(-1, -1, -1000, new Timestamp(0)));

        // Populate the stationSpeeds map with default values
        for (Map.Entry<Integer, StationData> entry: stationDataMap.entrySet()) {
            this.stationSpeeds.put(entry.getKey(), new ArrayList<>());
            double progress = stationDataMap.get(entry.getKey()).progressToNext();
            double time = entry.getValue().distanceToNext() / AVG_SPEED;
            this.stationSpeeds.get(entry.getKey()).add(progress / time);
        }
    }

    public List<Position> update(List<Detection> detections) {
        if (teamId == 1) {
            logger.info("Updating");
        }
        boolean newStation = handleDetection(detections);
        if (!newStation) {
            return new ArrayList<>();
        }

        StationData station = stationDataMap.get(currentStation);
        long now = System.currentTimeMillis();
        List<Position> oldPositions = new ArrayList<>(positions.stream().filter(p -> p.timestamp() < now).toList());
        if (oldPositions.isEmpty()) {
            oldPositions.add(new Position(teamId, 0, 0, 0, 0));
        }
        Position lastPosition = oldPositions.get(oldPositions.size() - 1);
        long interval = now - lastPosition.timestamp();

        double maxDeviation = station.progressToNext() * 2;
        double position = normalize(lastPosition.progress() + lastPosition.speed() * interval + 0.5 * lastPosition.acceleration() * Math.pow(interval, 2));
        if (circularDistance(position, station.progress()) > maxDeviation) {
            // Don't let the animation deviate too much from the reality
            position = station.progress();
        }

        double progress = station.progressToNext();
        double time = station.progressToNext() / getMedianSpeed(currentStation);

        // We'll generate 3 positions
        // Part 1 -> (De)accelerate to try to sync the animation with the reality
        // Part 2 -> Opposite acceleration towards the median speed
        // Part 3 -> Continue at the median speed (no acceleration)
        // The goal of part 1 is to try and sync the animation with the reality
        // Part 2 will slow it down / speed it up back to its median (expected) speed
        // Part 3 continues at the median speed to avoid any lasting (de)acceleration if we don't receive anymore data

        // To simplify the calculation (no circular distance) we set the current position and timestamp to 0
        double p0 = 0;
        double v0 = lastPosition.speed() + 0.5 * lastPosition.acceleration() * Math.pow(interval, 2);
        double a0; // Unknown
        double t0 = 0;

        double p1 = p0 + 0.5 * progress;
        double v1; // Unknown
        double a1; // Unknown
        double t1 = t0 + 0.5 * time;

        double p2 = p1 + 0.4 * progress;
        double v2 = getMedianSpeed(currentStation);
        double a2 = 0;
        double t2 = t1 + 0.4 * time;

        // Calculate
        // a0 = (v1 - v0) / (t1 - t0)
        // a1 = (v2 - v1) / (t2 - t1)
        // v1

        // v1 can be found by summing the progress
        // p1 = p0 + v0 * (t1 - t0) + 0.5 * a0 * (t1 - t0)^2
        // p2 = p1 + v1 * (t2 - t1) + 0.5 * a1 * (t2 - t1)^2
        // p1 + p2 = ...
        // ...
        // v1 = (p2 - 0.5 * v0 * t1 - 0.5 * v2 * (t2 - t1)) / (0.5 * t2)
        v1 = (p2 - 0.5 * v0 * t1 - 0.5 * v2 * (t2 - t1)) / (0.5 * t2);
        a0 = (v1 - v0) / (t1 - t0);
        a1 = (v2 - v1) / (t2 - t1);

        // Re-add the position and timestamp
        p0 = normalize(position + p0);
        p1 = normalize(position + p1);
        p2 = normalize(position + p2);

        t0 = now + t0;
        t1 = now + t1;
        t2 = now + t2;

        positions.clear();
        positions.addAll(Arrays.asList(
                new Position(teamId, p0, v0, a0, (long) t0),
                new Position(teamId, p1, v1, a1, (long) t1),
                new Position(teamId, p2, v2, a2, (long) t2)
        ));
        if (teamId == 1) {
            logger.info(positions.toString());
        }

        return positions;
    }

    private boolean handleDetection(List<Detection> newDetections) {
        boolean newStation = false;

        newDetections.sort(Comparator.comparing(Detection::getTimestamp));
        for (Detection detection: newDetections) {
            if (!detection.getTimestamp().after(detections.getLast().getTimestamp())) {
                // Only keep newer detections
                continue;
            }

            detections.add(detection); // Newest detection is now at the end of the list

            if (detection.getStationId() == currentStation) {
                // We're already at this station
                continue;
            }

            // Filter out old detections
            long lastDetection = detections.getLast().getTimestamp().getTime();
            detections.removeIf(d -> lastDetection - d.getTimestamp().getTime() > INTERVAL);

            // Determine new position
            int newStationId = detections.stream().max(Comparator.comparing(Detection::getRssi)).get().getStationId(); // detections will at least contain the last detection
            if (currentStation != newStationId && stationAfter(newStationId)) {
                // New position!
                // Add new speed
                if (currentStationDetection != null && newStationId == stationDataMap.get(currentStation).nextStationId()) { //  Necessary for the first station switch
                    double progress = normalize(stationDataMap.get(newStationId).progress() - stationDataMap.get(currentStation).progress());
                    double time = detection.getTimestamp().getTime() - currentStationDetection.getTimestamp().getTime();
                    stationSpeeds.get(currentStation).add(progress / time);
                }

                // Update station variables
                currentStation = newStationId;
                currentStationDetection = detection;
                newStation = true;
            }
        }

        return newStation;
    }

    private boolean stationAfter(int newStationId) {
        if (currentStationDetection == null) {
            return true;
        }

        int stations = stationDataMap.size();
        return (((stationDataMap.get(newStationId).index() - stationDataMap.get(currentStation).index()) % stations) + stations) % stations < 4;
    }

    private double getMedianSpeed(int stationId) {
        List<Double> times = stationSpeeds.get(stationId);
        if (times.size() > MAX_TIMES) {
            times.subList(0, times.size() - MAX_TIMES).clear();
        }

        List<Double> copy = new ArrayList<>(times);
        Collections.sort(copy);

        double median;
        if (copy.size() % 2 == 0) {
            median = (copy.get(copy.size() / 2) + copy.get(copy.size() / 2 - 1)) / 2;
        } else {
            median = copy.get(copy.size() / 2);
        }

        return median;
    }

    private double circularDistance(double a, double b) {
        double diff = Math.abs(a - b);
        return Math.min(diff, 1 - diff);
    }

    private double normalize(double amount) {
        return ((amount % 1) + 1) % 1;
    }
}
