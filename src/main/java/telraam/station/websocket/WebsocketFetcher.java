package telraam.station.websocket;

import com.fasterxml.jackson.core.JsonProcessingException;
import lombok.AllArgsConstructor;
import org.jdbi.v3.core.Jdbi;
import com.fasterxml.jackson.databind.ObjectMapper;
import telraam.database.daos.BatonDAO;
import telraam.database.daos.DetectionDAO;
import telraam.database.daos.StationDAO;
import telraam.database.models.Detection;
import telraam.database.models.Station;
import telraam.logic.lapper.Lapper;
import telraam.logic.positioner.Positioner;
import telraam.station.Fetcher;
import telraam.station.models.RonnyDetection;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.*;
import java.sql.Timestamp;
import java.util.*;
import java.util.logging.Logger;

public class WebsocketFetcher implements Fetcher {
    private final Set<Lapper> lappers;
    private final Set<Positioner> positioners;
    private Station station;

    private final BatonDAO batonDAO;
    private final DetectionDAO detectionDAO;
    private final StationDAO stationDAO;

    private final HttpClient client = HttpClient.newHttpClient();
    private final Logger logger = Logger.getLogger(WebsocketFetcher.class.getName());

    public WebsocketFetcher(Jdbi database, Station station, Set<Lapper> lappers, Set<Positioner> positioners) {
        this.batonDAO = database.onDemand(BatonDAO.class);
        this.detectionDAO = database.onDemand(DetectionDAO.class);
        this.stationDAO = database.onDemand(StationDAO.class);
        this.lappers = lappers;
        this.positioners = positioners;

        this.station = station;
    }

    public void fetch() {
        logger.info("Running Fetcher for station(" + this.station.getId() + ")");
        ObjectMapper mapper = new ObjectMapper();

        //Update the station to account for possible changes in the database
        this.stationDAO.getById(station.getId()).ifPresentOrElse(
                station -> this.station = station,
                () -> this.logger.severe("Can't update station from database.")
        );

        //Get last detection id
        int lastDetectionId = 0;
        Optional<Detection> lastDetection = detectionDAO.latestDetectionByStationId(this.station.getId());
        if (lastDetection.isPresent()) {
            lastDetectionId = lastDetection.get().getRemoteId();
        }

        InitWSMessage wsMessage = new InitWSMessage(lastDetectionId);
        String wsMessageEncoded;
        try {
            wsMessageEncoded = mapper.writeValueAsString(wsMessage);
        } catch (JsonProcessingException e) {
            logger.severe(e.getMessage());
            try {
                Thread.sleep(Fetcher.ERROR_TIMEOUT_MS);
            } catch (InterruptedException ex) {
                logger.severe(ex.getMessage());
            }
            this.fetch();
            return;
        }

        //Create URL
        URI url;
        try {
            url = new URI(station.getUrl());
        } catch (URISyntaxException ex) {
            this.logger.severe(ex.getMessage());
            try {
                Thread.sleep(Fetcher.ERROR_TIMEOUT_MS);
            } catch (InterruptedException e) {
                logger.severe(e.getMessage());
            }
            this.fetch();
            return;
        }


        WebsocketClient websocketClient = new WebsocketClient(url);
        websocketClient.addOnOpenHandler(() -> {
            websocketClient.sendMessage(wsMessageEncoded);
        });
        websocketClient.addOnCloseHandler(() -> {
            this.logger.severe(String.format("Websocket for station %s got closed", station.getName()));
            try {
                Thread.sleep(Fetcher.ERROR_TIMEOUT_MS);
            } catch (InterruptedException e) {
                logger.severe(e.getMessage());
            }
            this.fetch();
        });
        websocketClient.addMessageHandler((String msg) -> {
            //Insert detections
            List<Detection> new_detections = new ArrayList<>();
            List<String> detection_mac_addresses = new ArrayList<>();

            try {
                List<RonnyDetection> detections = Arrays.asList(mapper.readValue(msg, RonnyDetection[].class));
                for (RonnyDetection detection : detections) {
                    new_detections.add(new Detection(
                            0,
                            station.getId(),
                            detection.rssi,
                            detection.battery,
                            detection.uptimeMs,
                            detection.id,
                            new Timestamp((long) (detection.detectionTimestamp * 1000)),
                            new Timestamp(System.currentTimeMillis())
                    ));
                    detection_mac_addresses.add(detection.mac);
                }
                if (!new_detections.isEmpty()) {
                    List<Detection> db_detections = detectionDAO.insertAllWithoutBaton(new_detections, detection_mac_addresses);
                    for(int i = 0; i < new_detections.size(); i++) {
                        Detection detection = new_detections.get(i);
                        Detection db_detection = db_detections.get(i);

                        detection.setBatonId(db_detection.getBatonId());
                        detection.setId(db_detection.getId());

                        lappers.forEach((lapper) -> lapper.handle(detection));
                        positioners.forEach(positioner -> positioner.handle(detection));
                    }
                }

                logger.finer("Fetched " + detections.size() + " detections from " + station.getName() + ", Saved " + new_detections.size());
            } catch (JsonProcessingException e) {
                logger.severe(e.getMessage());
            }
        });

        try {
            websocketClient.listen();
        } catch (RuntimeException ex) {
            this.logger.severe(ex.getMessage());
            try {
                Thread.sleep(Fetcher.ERROR_TIMEOUT_MS);
            } catch (InterruptedException e) {
                logger.severe(e.getMessage());
            }
            this.fetch();
            return;
        }
    }

    @AllArgsConstructor
    private static class InitWSMessage {
        public int lastId;
    }
}
