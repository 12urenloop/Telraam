package telraam.logic.lapper.slapper;

import io.dropwizard.jersey.setup.JerseyEnvironment;
import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.LapSourceDAO;
import telraam.database.models.Detection;
import telraam.database.models.LapSource;
import telraam.logic.lapper.Lapper;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

// Lapper that only uses a single sql query
public class Slapper implements Lapper {
    private final String SOURCE_NAME = "slapper";
    private final int DEBOUNCE_TIMEOUT = 10;
    private final ScheduledExecutorService scheduler;
    private boolean debounceScheduled;
    private final Logger logger;
    private final Jdbi jdbi;

    public Slapper(Jdbi jdbi) {
        this.jdbi = jdbi;
        this.scheduler = Executors.newScheduledThreadPool(1);
        this.logger = Logger.getLogger(Slapper.class.getName());
        this.debounceScheduled = false;

        // Get the lapSourceId, create the source if needed
        LapSourceDAO lapSourceDAO = jdbi.onDemand(LapSourceDAO.class);
        if (lapSourceDAO.getByName(SOURCE_NAME).isEmpty()) {
            lapSourceDAO.insert(new LapSource(SOURCE_NAME));
        }
    }

    @Override
    public void handle(Detection msg) {
        if (!this.debounceScheduled) {
            this.debounceScheduled = true;
            this.scheduler.schedule(() -> {
                try {
                    this.calculateLaps();
                } catch (Exception e) {
                    logger.severe(e.getMessage());
                }
                this.debounceScheduled = false;
            }, DEBOUNCE_TIMEOUT, TimeUnit.SECONDS);
        }
    }

    private void calculateLaps() {
        logger.info("Slapper: Calculating laps...");
        this.jdbi.useHandle(handle -> handle.execute(
                        """
                                WITH switchovers AS (
                                	SELECT teamid AS team_id,
                                		newbatonid,
                                		timestamp AS start_timestamp,
                                		COALESCE(
                                			LEAD(timestamp) OVER (PARTITION BY teamid ORDER BY timestamp),
                                			timestamp + INTERVAL '1 MONTH'
                                		) AS next_baton_switch
                                	FROM batonswitchover
                                ),
                                team_detections AS (
                                	SELECT MIN(station_id) AS station_id,
                                		MAX(rssi) as rssi,
                                		date_trunc('second', timestamp) AS timestamp_seconds,
                                		team_id
                                	FROM detection d
                                	LEFT JOIN switchovers s ON d.baton_id = s.newbatonid
                                		AND d.timestamp BETWEEN s.start_timestamp AND s.next_baton_switch
                                	WHERE station_id NOT BETWEEN 3 AND 5
                                		AND rssi > -84
                                		AND team_id IS NOT NULL
                                	GROUP BY date_trunc('second', timestamp), team_id
                                ),
                                start_times AS (
                                	SELECT DISTINCT ON (team_id) team_id,
                                		timestamp_seconds AS start_seconds
                                	FROM team_detections
                                	WHERE station_id BETWEEN 2 AND 3
                                	ORDER BY team_id, timestamp_seconds
                                ),
                                new_laps AS (
                                	SELECT previous.team_id,
                                		timestamp_seconds
                                	FROM (
                                		SELECT *,
                                			LAG(station_id) OVER (
                                				PARTITION BY team_id
                                				ORDER BY timestamp_seconds
                                			) AS prev_station_id
                                		FROM team_detections
                                	) AS previous
                                	LEFT JOIN start_times s_t
                                	ON previous.team_id = s_t.team_id
                                	WHERE station_id - prev_station_id < -4
                                		AND timestamp_seconds > start_seconds
                                ),
                                cst_source_id AS (
                                	SELECT COALESCE(id, -1) AS source_id
                                	FROM lap_source
                                	WHERE name ILIKE 'slapper'
                                ),
                                deletions AS (
                                	DELETE FROM lap l
                                	WHERE lap_source_id = (SELECT source_id FROM cst_source_id)
                                		AND NOT EXISTS (
                                			SELECT 1
                                			FROM new_laps n_l
                                			WHERE l.team_id = n_l.team_id
                                				AND l.timestamp = n_l.timestamp_seconds
                                		)
                                )
                                INSERT INTO lap (team_id, timestamp, lap_source_id)
                                SELECT team_id,
                                	timestamp_seconds,
                                	source_id
                                FROM new_laps n_l, cst_source_id
                                WHERE NOT EXISTS (
                                	SELECT 1
                                	FROM lap l, cst_source_id
                                	WHERE l.lap_source_id = source_id
                                		AND l.team_id = n_l.team_id
                                		AND l.timestamp = n_l.timestamp_seconds
                                )
                                """
                )
        );
        logger.info("Slapper: Done calculating laps");
    }

    @Override
    public void registerAPI(JerseyEnvironment jersey) {

    }
}
