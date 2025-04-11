package telraam.logic.positioner.nostradamus.v2;

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
    private final int MAX_TIMES = 20; // Amount of speeds to keep track of to determine the median
    private  final int teamId;
    public AtomicInteger batonId;
    private final double maxSpeed;
    private final Map<Integer, StationData> stationDataMap; // Map from station id to StationData
    private final Map<Integer, List<Double>> stationSpeeds; // Avg speed (progress / ms) to go from a stationId to the next
    private int currentStation; // Current station id
    private Position lastPosition;
    private final Queue<Position> positions;

    private final LinkedList<Detection> detections;
    private Detection currentStationDetection;

    public TeamHandler(int teamId, AtomicInteger batonId, double maxSpeed, Map<Integer, StationData> stationDataMap) {
        this.teamId = teamId;
        this.batonId = batonId;
        this.maxSpeed = maxSpeed;
        this.stationDataMap = stationDataMap;

        this.stationSpeeds = new HashMap<>();
        this.positions = new ArrayDeque<>();
        this.detections = new LinkedList<>();

        this.currentStation = -1;
        this.lastPosition = new Position(0, 0, 0, 0);
        this.detections.add(new Detection(-1, -1, -1000, new Timestamp(0)));

        // Populate the stationSpeeds map with default values
        for (Map.Entry<Integer, StationData> entry: stationDataMap.entrySet()) {
            this.stationSpeeds.put(entry.getKey(), new ArrayList<>());
            double progress = stationDataMap.get(entry.getKey()).progressToNext();
            double time = entry.getValue().distanceToNext() / AVG_SPEED;
            this.stationSpeeds.get(entry.getKey()).add(progress / time);
        }
    }

    public void update(List<Detection> detections) {
        boolean newStation = handleDetection(detections);
        if (!newStation) {
            return;
        }

        StationData station = stationDataMap.get(currentStation);
        long timestamp = System.currentTimeMillis();

        double currentProgress = normalize(lastPosition.progress() + lastPosition.speed() * (timestamp - lastPosition.timestamp())); // Where is the animation now

        double maxDeviation = station.progressToNext();
        if (circularDistance(currentProgress, station.progress()) > maxDeviation) {
            // Don't let the animation deviate too much from the reality
            currentProgress = station.progress();
        }

        long intervalTime = (long) (station.progressToNext() / getMedianSpeed(currentStation)); // How many ms until it should reach the next station
        double goalProgress = normalize(station.progress() + station.progressToNext()); // Where is the next station
        double speed = normalize(goalProgress - currentProgress) / intervalTime;

        if (speed > maxSpeed) {
            // Sanity check
            currentProgress = stationDataMap.get(currentStation).progress();
            speed = getMedianSpeed(currentStation);
        }

        positions.clear();
        positions.add(new Position(teamId, currentProgress, speed, timestamp));
    }

    public Position getPosition() {
        if (!positions.isEmpty()) {
            lastPosition = positions.poll();
            return lastPosition;
        }

        return null;
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
                // We've already determined that we have arrived at this station
                continue;
            }

            // Filter out old detections
            long lastDetection = detections.getLast().getTimestamp().getTime();
            detections.removeIf(d -> lastDetection - d.getTimestamp().getTime() > INTERVAL);

            // Determine new position
            int newStationId = detections.stream().max(Comparator.comparing(Detection::getRssi)).get().getStationId(); // detections will at least contain the last detection
            if (currentStation != newStationId) {
                // New position!
                // Add new speed
                if (currentStationDetection != null) { //  Necessary for the first station switch
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
