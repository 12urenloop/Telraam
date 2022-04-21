package telraam.station;

import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.BatonDAO;
import telraam.database.daos.DetectionDAO;
import telraam.database.daos.StationDAO;
import telraam.database.models.Baton;
import telraam.database.models.Detection;
import telraam.database.models.Station;
import telraam.logic.Lapper;

import java.io.IOException;
import java.net.ConnectException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpConnectTimeoutException;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.sql.Timestamp;
import java.time.Duration;
import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public class Fetcher {
    private final Set<Lapper> lappers;
    private Station station;

    private final BatonDAO batonDAO;
    private final DetectionDAO detectionDAO;
    private final StationDAO stationDAO;

    private final HttpClient client = HttpClient.newHttpClient();
    private final Logger logger = Logger.getLogger(Fetcher.class.getName());

    //Timeout to wait for before sending the next request after an error.
    private final static int ERROR_TIMEOUT_MS = 2000;
    //Timeout for a request to a station.
    private final static int REQUEST_TIMEOUT_S = 10;
    //Full batch size, if this number of detections is reached, more are probably available immediately.
    private final static int FULL_BATCH_SIZE = 1000;
    //Timeout when result has less than a full batch of detections.
    private final static int IDLE_TIMEOUT_MS = 4000; // Wait 4 seconds


    public Fetcher(Jdbi database, Station station, Set<Lapper> lappers) {
        this.batonDAO = database.onDemand(BatonDAO.class);
        this.detectionDAO = database.onDemand(DetectionDAO.class);
        this.stationDAO = database.onDemand(StationDAO.class);
        this.lappers = lappers;

        this.station = station;
    }

    public void fetch() {
        logger.info("Running Fetcher for station(" + this.station.getId() + ")");
        JsonBodyHandler<RonnyResponse> bodyHandler = new JsonBodyHandler<>(RonnyResponse.class);

        while (true) {
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

            //Create URL
            URI url;
            try {
                url = new URI(station.getUrl() + "/detections/" + lastDetectionId);
            } catch (URISyntaxException ex) {
                this.logger.severe(ex.getMessage());
                try {
                    Thread.sleep(Fetcher.ERROR_TIMEOUT_MS);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                continue;
            }

            //Create request
            HttpRequest request;
            try {
                request = HttpRequest.newBuilder()
                        .uri(url)
                        .version(HttpClient.Version.HTTP_1_1)
                        .timeout(Duration.ofSeconds(Fetcher.REQUEST_TIMEOUT_S))
                        .build();
            } catch (IllegalArgumentException ex) {
                ex.printStackTrace();
                try {
                    Thread.sleep(Fetcher.ERROR_TIMEOUT_MS);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                continue;
            }

            //Do request
            HttpResponse<Supplier<RonnyResponse>> response;
            try {
                try {
                    response = this.client.send(request, bodyHandler);
                } catch (ConnectException | HttpConnectTimeoutException ex) {
                    this.logger.severe("Could not connect to " + request.uri());
                    Thread.sleep(Fetcher.ERROR_TIMEOUT_MS);
                    continue;
                } catch (IOException ex) {
                    ex.printStackTrace();
                    Thread.sleep(Fetcher.ERROR_TIMEOUT_MS);
                    continue;
                }
            } catch (InterruptedException ex) {
                ex.printStackTrace();
                continue;
            }

            //Check response state
            if (response.statusCode() != 200) {
                this.logger.warning(
                        "Unexpected status code(" + response.statusCode() + ") when requesting " + url + " for station(" + this.station.getName() + ")"
                );
                continue;
            }

            //Fetch all batons and create a map by batonMAC
            Map<String, Baton> baton_mac_map = batonDAO.getAll().stream().collect(Collectors.toMap(Baton::getMac, Function.identity()));

            //Insert detections
            List<Detection> new_detections = new ArrayList<>();
            List<RonnyDetection> detections = response.body().get().detections;
            for (RonnyDetection detection : detections) {
                Optional.ofNullable(baton_mac_map.getOrDefault(detection.mac, null)).ifPresent(value -> {
                    new_detections.add(new Detection(
                            value.getId(),
                            station.getId(),
                            detection.rssi,
                            detection.battery,
                            detection.uptimeMs,
                            detection.id,
                            new Timestamp(detection.detectionTimestamp * 1000)
                    ));
                });
            }
            if (!new_detections.isEmpty()) {
                detectionDAO.insertAll(new_detections);
                new_detections.forEach((detection) -> lappers.forEach((lapper) -> lapper.handle(detection)));
            }

            this.logger.finer("Fetched " + detections.size() + " detections from " + station.getName() + ", Saved " + new_detections.size());

            //If few detections are retrieved from the station, wait for some time.
            if (detections.size() < Fetcher.FULL_BATCH_SIZE) {
                try {
                    Thread.sleep(Fetcher.IDLE_TIMEOUT_MS);
                } catch (InterruptedException ex) {
                    ex.printStackTrace();
                }
            }
        }
    }
}