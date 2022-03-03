package telraam.station;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class Fetcher {
    private static HttpClient client = HttpClient.newHttpClient();

    private List<URI> stations;
    private List<AtomicBoolean> busy;
    private List<Consumer<Detection>> detectionHandlers;
    private int current = 0;

    public Fetcher() {
        this.stations = new ArrayList<>();
        this.detectionHandlers = new ArrayList<>();
    }

    public void addStation(URI url) {
        this.stations.add(url);
        this.busy.add(new AtomicBoolean(false));
    }

    public void fetch() {
        // WE HAVE STATIONS
        if (!this.stations.isEmpty()) {
            if (!this.busy.get(this.current).get()) {
                int cur = this.current;
                Runnable end = () -> {
                    this.busy.get(cur).set(false);
                };

                get(this.stations.get(this.current), this::handleError, this::handleDetection, end);
            }
        }
        this.current++;
    }

    public Runnable start() {
        return () -> {
            while (true) {
                this.fetch();
                try {
                    Thread.sleep(500);
                } catch (InterruptedException ex) {
                }
            }
        };
    }

    private Void handleDetection(HttpResponse<Supplier<Detections>> resp) {
        if (resp.statusCode() != 200)
            return null;
        Detections detections = resp.body().get();

        for (Detection detection : detections.detections()) {
            detection.setStationId(detections.stationId());

            for (Consumer<Detection> h : this.detectionHandlers) {
                h.accept(detection);
            }
        }

        return null;
    }

    private void handleError(Throwable err) {
        System.err.println(err);
    }

    protected static void get(URI url, Consumer<Throwable> onError,
            Consumer<HttpResponse<Supplier<Detections>>> onDetections, Runnable after) {
        // create a request
        var request = HttpRequest.newBuilder(url).build();
        var bodyHandler = new JsonBodyHandler<>(Detections.class);

        client.sendAsync(request, bodyHandler).thenApply(x -> {
            onDetections.accept(x);
            return null;
        }).whenComplete((_void, e) -> {
            if (e != null)
                onError.accept(e);
            after.run();
        });
    }
}