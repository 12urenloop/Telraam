package telraam.station;

public interface Fetcher {
    //Timeout to wait for before sending the next request after an error.
    int ERROR_TIMEOUT_MS = 2000;
    //Timeout for a request to a station.
    int REQUEST_TIMEOUT_S = 10;
    //Full batch size, if this number of detections is reached, more are probably available immediately.
    int FULL_BATCH_SIZE = 1000;
    //Timeout when result has less than a full batch of detections.
    int IDLE_TIMEOUT_MS = 4000; // Wait 4 seconds

    void fetch();
}
