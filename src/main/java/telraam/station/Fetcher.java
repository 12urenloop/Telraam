package telraam.station;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

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

    private FetchConfig config;
    private List<Station> stations;
    private List<AtomicBoolean> busy;
    private List<Consumer<Detection>> detectionHandlers;
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

    public void addDetectionHanlder(Consumer<Detection> handler) {
        this.detectionHandlers.add(handler);
    }

    public void addStation(String urlBase) {
        this.stations.add(new Station(urlBase));
        this.busy.add(new AtomicBoolean(false));
    }

    public void fetchAll() {
        for (int i = 0; i < this.stations.size(); i++) {
            if (!this.busy.get(i).get()) {
                var ind = i;
                Runnable end = () -> {
                    this.busy.get(ind).set(false);
                };

                get(this.stations.get(ind), this::handleError, this::handleDetection, end);
            }
        }

    }

    public void fetch() {
        if (!this.stations.isEmpty()) {
            if (!this.busy.get(this.current).get()) {
                int cur = this.current;
                Runnable end = () -> {
                    this.busy.get(cur).set(false);
                };

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
                switch (config.waitPolicy()) {
                    case PER_STATION -> this.fetch();
                    case PER_STATION_BLOCK -> this.fetchAll();
                }
                ;

                try {
                    Thread.sleep(this.config.waitMs());
                } catch (InterruptedException ex) {
                }
            }
        };
    }

    private Void handleDetection(Detections detections) {
        for (Detection detection : detections.getDetections()) {
            detection.setStationId(detections.getStationId());

            for (Consumer<Detection> h : this.detectionHandlers) {
                h.accept(detection);
            }
        }

        return null;
    }

    private void handleError(Throwable err) {
        System.err.println(err);
    }

    protected static void get(Station station, Consumer<Throwable> onError,
            Consumer<Detections> onDetections, Runnable after) {
        // create a request
        var request = HttpRequest.newBuilder(station.getUri()).build();
        var bodyHandler = new JsonBodyHandler<>(Detections.class);

        client.sendAsync(request, bodyHandler).thenApply(x -> {
            if (x.statusCode() == 200) {
                var detections = x.body().get();
                onDetections.accept(detections);
                detections.getDetections().stream()
                        .map(Detection::getId)
                        .forEach(station::setLastSeenId);
            }

            return null;
        }).whenComplete((_void, e) -> {
            if (e != null)
                onError.accept(e);
            after.run();
        });
    }
}