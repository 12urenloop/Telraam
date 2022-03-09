package telraam.station;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.logging.Level;
import java.util.logging.Logger;

import telraam.station.FetchConfig.WaitBetween;

public class Fetcher {
    private static class Station {
        private String uriBase;
        private int lastSeenId;

        public Station(String uri) {
            this.uriBase = uri;
            this.lastSeenId = 0;
        }

        public URI getUri() {
            return URI.create(this.uriBase + this.lastSeenId);
        }

        public void setLastSeenId(int id) {
            if (id > this.lastSeenId)
                this.lastSeenId = id;
        }
    }

    private static HttpClient client = HttpClient.newHttpClient();
    private static Logger logger = Logger.getLogger(Fetcher.class.getName());

    private FetchConfig config;
    private List<Station> stations;
    private List<AtomicBoolean> busy;
    private List<Consumer<RonnyDetection>> detectionHandlers;
    private int current = 0;

    public Fetcher() {
        this(new FetchConfig(WaitBetween.PER_STATION_BLOCK, 200));
    }

    public Fetcher(FetchConfig config) {
        this.stations = new ArrayList<>();
        this.busy = new ArrayList<>();
        this.detectionHandlers = new ArrayList<>();
        this.config = config;
    }

    public void setConfig(FetchConfig config) {
        this.config = config;
    }

    public void addDetectionHanlder(Consumer<RonnyDetection> handler) {
        this.detectionHandlers.add(handler);
    }

    public void addStation(String urlBase) {
        this.stations.add(new Station(urlBase));
        this.busy.add(new AtomicBoolean(false));
    }

    public void fetchAll() {
        for (int i = 0; i < this.stations.size(); i++) {
            if (!this.busy.get(i).get()) {
                Runnable end = this.getEnd(i);
                get(this.stations.get(i), this::handleError, this::handleDetection, end);
            }
        }
    }

    public void fetch() {
        if (!this.stations.isEmpty()) {
            if (!this.busy.get(this.current).get()) {
                Runnable end = this.getEnd(this.current);
                get(this.stations.get(this.current), this::handleError, this::handleDetection, end);
            }

            this.current++;
            if (this.stations.size() <= this.current)
                this.current = 0;
        }
    }

    public Runnable start() {
        return () -> {
            while (true) {
                if (config.waitPolicy() == WaitBetween.PER_STATION)
                    this.fetch();
                else if (config.waitPolicy() == WaitBetween.PER_STATION_BLOCK)
                    this.fetchAll();

                try {
                    Thread.sleep(this.config.waitMs());
                } catch (InterruptedException ex) {
                    return;
                }
            }
        };
    }

    private Runnable getEnd(int index) {
        return () -> this.busy.get(index).set(false);
    }

    private void handleDetection(RonnyResponse detections) {
        for (RonnyDetection detection : detections.getDetections()) {
            detection.setStationRonnyName(detections.getStationRonnyName());

            for (Consumer<RonnyDetection> h : this.detectionHandlers) {
                h.accept(detection);
            }
        }
    }

    private void handleError(Throwable err) {
        logger.log(Level.WARNING, "Error", err);
    }

    protected static void get(Station station, Consumer<Throwable> onError,
            Consumer<RonnyResponse> onDetections, Runnable after) {
        // create a request
        var request = HttpRequest.newBuilder(station.getUri()).build();
        var bodyHandler = new JsonBodyHandler<>(RonnyResponse.class);

        client.sendAsync(request, bodyHandler).thenApply(x -> {
            if (x.statusCode() == 200) {
                var detections = x.body().get();
                onDetections.accept(detections);
                detections.getDetections().stream()
                        .map(RonnyDetection::getId)
                        .forEach(station::setLastSeenId);
            }

            return null;
        }).whenComplete((v, e) -> {
            if (e != null)
                onError.accept(e);
            after.run();
        });
    }
}