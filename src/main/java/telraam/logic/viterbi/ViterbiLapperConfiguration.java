package telraam.logic.viterbi;

public class ViterbiLapperConfiguration {
    public int TRACK_LENGTH;                // In meters
    public int[] SECTOR_STARTS;             // In meters, the final sector ends at TRACK_LENGTH
    public double AVERAGE_RUNNER_SPEED;     // In meters per second
    public double DETECTIONS_PER_SECOND;    // The number of detections per station, per second
    public double STATION_RANGE_SIGMA;      // The sigma parameter of the detection probability of the stations
    public double RESTART_PROBABILITY;      // The probability that the runners will start the race in a different spot than the start/finish line (should only happen on complete restarts)
    public int DEBOUNCE_TIMEOUT;            // The amount of time detections are debounced for in seconds

    // chance for detection in segment corresponding to a station
    public double SAME_STATION_DETECTION_CHANCE;

    // the probability that you will be detected at any station ("noise" detections)
    public double BASE_DETECTION_CHANCE;

    // the probability that an individual station is down at any moment in time
    // ~= downtime / total time.
    public double BROKEN_STATION_PROBABILITY;

    // The amount of times we expect a runner to be detected when passing by a station, given that the station is alive.
    //
    // The higher this parameter, the more 'evidence' (= detections) the system will require
    // in order to change the predicted location - so, the predictions will be less sensitive to noise detections
    // at different stations.
    // Consequently, it is better to under-estimate this parameter than to over-estimate it.
    public double EXPECTED_NUM_DETECTIONS;

    public ViterbiLapperConfiguration() {
        this.TRACK_LENGTH = 100;
        this.SECTOR_STARTS = new int[]{0, 100, 150, 250, 350};
        this.AVERAGE_RUNNER_SPEED = 3.2;
        this.DETECTIONS_PER_SECOND = 1;
        this.STATION_RANGE_SIGMA = 50;
        this.RESTART_PROBABILITY = 0.001;
        this.DEBOUNCE_TIMEOUT = 10;

        this.SAME_STATION_DETECTION_CHANCE = 0.8;
        this.BASE_DETECTION_CHANCE = 0.1;
        this.EXPECTED_NUM_DETECTIONS = 8;
        this.BROKEN_STATION_PROBABILITY = 0.01;
    }
}
