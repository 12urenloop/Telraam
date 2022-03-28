package telraam.station;

import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.BatonDAO;
import telraam.database.daos.DetectionDAO;
import telraam.database.daos.StationDAO;
import telraam.database.models.Detection;
import telraam.database.models.Station;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.logging.Logger;

public class Fetcher {
    private Station station;

    private final BatonDAO batonDAO;
    private final DetectionDAO detectionDAO;
    private final StationDAO stationDAO;

    private final HttpClient client = HttpClient.newHttpClient();
    private final Logger logger = Logger.getLogger(Fetcher.class.getName());


    public Fetcher(Jdbi database, Station station) {
        this.batonDAO = database.onDemand(BatonDAO.class);
        this.detectionDAO = database.onDemand(DetectionDAO.class);
        this.stationDAO = database.onDemand(StationDAO.class);

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
                lastDetectionId = lastDetection.get().getId();
            }

            //Create URL
            URI url;
            try {
                url = new URI(station.getUrl() + "/detections/" + lastDetectionId);
            } catch (URISyntaxException ex) {
                this.logger.severe(ex.getMessage());
                continue; //TODO: add timeout
            }

            //Create request
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(url)
                    .version(HttpClient.Version.HTTP_1_1)
                    .build();

            //Do request
            HttpResponse<Supplier<RonnyResponse>> response;
            try {
                response = this.client.send(request, bodyHandler);
            } catch (IOException ex) {
                this.logger.severe(ex.getMessage());
                continue;
            } catch (InterruptedException ex) {
                this.logger.warning(ex.getMessage());
                return; //Interrupted so stopping the thread
            }

            //Check response state
            if (response.statusCode() != 200) {
                this.logger.warning(
                        "Unexpected status code(" + response.statusCode() + ") for station(" + this.station.getName() + ")"
                );
                continue;
            }

            //Insert detections
            List<RonnyDetection> detections = response.body().get().detections;
            for (RonnyDetection detection : detections) {
                batonDAO.getByMAC(detection.mac).ifPresent(value -> {
                    detectionDAO.insert(new Detection(
                            value.getId(),
                            station.getId(),
                            detection.rssi,
                            detection.battery,
                            detection.uptimeMs,
                            detection.id,
                            new Timestamp(detection.detectionTimestamp)
                    ));
                });
            }
        }
    }
}