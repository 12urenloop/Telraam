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
    // chance for detection in a different section
    public double DIFFERENT_STATION_DETECTION_CHANCE;

    // how much detections you expect to get (assuming all are succesful)
    // when a runner passes a station
    public double EXPECTED_NUM_DETECTIONS;

    public ViterbiLapperConfiguration() {
        this.TRACK_LENGTH = 100;
        this.SECTOR_STARTS = new int[]{0, 100, 150, 250, 350};
        this.AVERAGE_RUNNER_SPEED = 3.2;
        this.DETECTIONS_PER_SECOND = 1;
        this.STATION_RANGE_SIGMA = 50;
        this.RESTART_PROBABILITY = 0.001;
        this.DEBOUNCE_TIMEOUT = 10;

        this.SAME_STATION_DETECTION_CHANCE = 0.5;
        this.DIFFERENT_STATION_DETECTION_CHANCE = 0.1;
        this.EXPECTED_NUM_DETECTIONS = 8;
    }
}
