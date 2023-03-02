package telraam.logic.robustLapper.robustLapperIncremental;

import io.dropwizard.jersey.setup.JerseyEnvironment;
import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.*;
import telraam.database.models.*;
import telraam.logic.Lapper;

import java.sql.Timestamp;
import java.util.*;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.stream.Collectors;

// 3 Potential problems:
// 1: Telraam gets restarted
//          -> lastDetection & lastSwitchoverId are set back to 0 and all previous laps get deleted
// 2: A switchover gets added with a timestamp earlier than the last detection that was handled ergo some detections ended up in the wrong team
//          -> processData checks for switchover timestamp. If it's earlier it fetches all detections since second last registered lap before the switchover timestamp
//          -> team class gets a detection with timestamp earlier than its latest detection, deletes all laps since then and fetches the detections again
// 3: Some detections don't get added in chronological order (e.g. temporarily ronny disconnect)
//          -> See second point for 2:

// Implement Lapper for easier use in App and Fetcher
public class RobustLapperIncremental implements Lapper {

    private final Jdbi jdbi;

    // Data processing variables
    private final int MIN_RSSI = -85;                                   // Min RSSI
    private Detection lastDetection = new Detection(-1, new Timestamp(0));  // Last detection
    private int lastSwitchoverId = -1;                                  // ID of last switchover
    private Map<Integer, Team> teamById;                                // Map teamId -> team
    private Map<Integer, RobustLapperTeam> teamLappers;                 // Map teamId -> RobustLapperTeam class
    private Map<Integer, List<Detection>> teamDetections;               // Map teamId -> their detections

    // Logger
    private final Logger logger;

    // Lap source variables
    private final String SOURCE_NAME = "robust-lapper-incremental";     // Name of lap source
    private int lapSourceId;                                            // ID in database

    // Scheduler variables
    private final int DEBOUNCE_TIMEOUT = 10;
    private final ScheduledExecutorService scheduler;
    private boolean debounceScheduled;

    public RobustLapperIncremental(Jdbi jdbi) {
        this.jdbi = jdbi;

        this.logger = Logger.getLogger(RobustLapperIncremental.class.getName());

        this.scheduler = Executors.newScheduledThreadPool(1);
        this.debounceScheduled = false;

        // Get the lapSourceId, create the source if needed
        LapSourceDAO lapSourceDAO = jdbi.onDemand(LapSourceDAO.class);
        lapSourceDAO.getByName(SOURCE_NAME).ifPresentOrElse(
                lapSource -> this.lapSourceId = lapSource.getId(),
                () -> this.lapSourceId = lapSourceDAO.insert(new LapSource(SOURCE_NAME))
        );

        setTeamVariables();
        deleteLaps();
    }

    // Set all team variables according to the data in the database
    private void setTeamVariables() {
        TeamDAO teamDAO = jdbi.onDemand(TeamDAO.class);
        List<Team> teams = teamDAO.getAll();

        teamById = teams.stream().collect(Collectors.toMap(Team::getId, team -> team));
        teamLappers = teams.stream().collect(Collectors.toMap(Team::getId, team -> new RobustLapperTeam(jdbi, team.getId(), lapSourceId)));
        teamDetections = teams.stream().collect(Collectors.toMap(Team::getId, team -> new ArrayList<>()));
    }

    // Deletes all previous lap records in db with the same lapSourceId
    private void deleteLaps() {
        LapDAO lapDAO = jdbi.onDemand(LapDAO.class);
        lapDAO.deleteByLapSourceId(lapSourceId);
    }

    // Retrieve and process all new detections and switchovers from database
    private void processData() {
        logger.info("RobustLapper: Calculating laps...");

        DetectionDAO detectionDAO = jdbi.onDemand(DetectionDAO.class);
        BatonSwitchoverDAO batonSwitchoverDAO = jdbi.onDemand(BatonSwitchoverDAO.class);

        // Get new detections and switchover
        List<Detection> detections = detectionDAO.getSinceIdWithMinRssi(lastDetection.getId(), MIN_RSSI);
        List<BatonSwitchover> switchovers = batonSwitchoverDAO.getSinceId(lastSwitchoverId);

        // Check if a switchover happened before the last processed detection
        if ((! switchovers.isEmpty()) && switchovers.get(0).getTimestamp().before(lastDetection.getTimestamp())) {
            // At least one did so add all detections that could influence a lap time
            getDataBeforeSwitchover(detections, detectionDAO, switchovers);
            // Get rid of duplicates and sort again
            Set<Detection> detectionSet = new HashSet<>(detections);
            detections.clear();
            detections.addAll(detectionSet);
            detections.sort(Comparator.comparing(Detection::getTimestamp));
        }

        // Divide detections according to which team it belongs to
        Map<Integer, Team> batonTeam = new HashMap<>();
        teamDetections.forEach((k, v) -> v.clear());

        int switchoverIndex = 0;
        for (Detection detection : detections) {
            // Switch teams batons if it happened before the current detection
            while (switchoverIndex < switchovers.size() && switchovers.get(switchoverIndex).getTimestamp().before(detection.getTimestamp())) {
                BatonSwitchover switchover = switchovers.get(switchoverIndex);
                batonTeam.put(switchover.getNewBatonId(), teamById.get(switchover.getTeamId()));
                batonTeam.remove(switchover.getPreviousBatonId());
                switchoverIndex++;
            }

            // Check if detection belongs to a team
            if (batonTeam.containsKey(detection.getBatonId())) {
                if (! teamDetections.containsKey(batonTeam.get(detection.getBatonId()).getId())) {
                    teamDetections.put(batonTeam.get(detection.getBatonId()).getId(), new ArrayList<>());
                }
                List<Detection> currentDetections = teamDetections.get(batonTeam.get(detection.getBatonId()).getId());
                // If team already has a detection for that timestamp keep the one with the strongest signal
                if (! currentDetections.isEmpty() && currentDetections.get(currentDetections.size() - 1).getTimestamp().compareTo(detection.getTimestamp()) == 0) {
                    if (currentDetections.get(currentDetections.size() - 1).getRssi() < detection.getRssi()) {
                        currentDetections.remove(currentDetections.size() - 1);
                        currentDetections.add(detection);
                    }
                } else {
                    currentDetections.add(detection);
                }
            }
        }

        // If detections isn't empty start giving each team its own list of detections
        if (! detections.isEmpty()) {
            lastDetection = detections.get(detections.size() - 1);
            if (! switchovers.isEmpty()) {
                lastSwitchoverId = switchovers.get(switchovers.size() - 1).getId();
            }
            startTeamCalculatingLaps();
        }

        logger.info("RobustLapper: Done calculating laps");
    }

    // Get all data before a switchover happened that could influence a lap time
    private void getDataBeforeSwitchover(List<Detection> detections, DetectionDAO detectionDAO, List<BatonSwitchover> switchovers) {
        LapDAO lapDAO = jdbi.onDemand(LapDAO.class);

        int i = 0;
        List<Integer> handledTeams = new ArrayList<>();
        while (i < switchovers.size() && switchovers.get(i).getTimestamp().before(lastDetection.getTimestamp())) {
            BatonSwitchover switchover = switchovers.get(i);
            if (! handledTeams.contains(switchover.getTeamId())) {
                handledTeams.add(switchover.getTeamId());
                Optional<Lap> secondLastLapOptional = lapDAO.getTeamSecondLastLapBefore(switchover.getTeamId(), switchover.getTimestamp(), lapSourceId);
                Timestamp timestamp;
                if (secondLastLapOptional.isPresent()) {
                    timestamp = secondLastLapOptional.get().getTimestamp();
                } else {
                    timestamp = new Timestamp(0);
                }
                detections.addAll(detectionDAO.getAfterTimestampFilterTeamId(switchover.getTeamId(), timestamp, MIN_RSSI));
            }
        }
    }

    // For each team with detections start a thread and wait until it finished
    private void startTeamCalculatingLaps() {
        List<Thread> threads = new ArrayList<>();
        for (Map.Entry<Integer, List<Detection>> entry : teamDetections.entrySet()) {
            if (! teamLappers.containsKey(entry.getKey())) {
                // Don't have a RobustLapperTeam yet so make one
                teamLappers.put(entry.getKey(), new RobustLapperTeam(jdbi, entry.getKey(), lapSourceId));
            }

            if (! entry.getValue().isEmpty()) {
                RobustLapperTeam lapperTeam = teamLappers.get(entry.getKey());
                lapperTeam.setDetections(entry.getValue());
                Thread thread = new Thread(lapperTeam);
                threads.add(thread);
                thread.start();
            }
        }

        // Wait until every thread is finished
        for (Thread thread : threads) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                logger.severe("RobustLapper: Thread InterruptedException.");
            }
        }
    }

    // Called by Fetcher whenever there are new detections
    @Override
    public void handle(Detection detection) {
        if (detection.getRssi() < MIN_RSSI) {
            return;
        }
        if (!debounceScheduled) {
            debounceScheduled = true;
            scheduler.schedule(() -> {
                try {
                    processData();
                } catch (Exception e) {
                    logger.severe(e.getMessage());
                }
                debounceScheduled = false;
            }, DEBOUNCE_TIMEOUT, TimeUnit.SECONDS);
        }
    }

    // Not used
    @Override
    public void registerAPI(JerseyEnvironment jersey) {

    }
}
