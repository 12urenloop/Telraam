package telraam.logic.viterbi;

public class ViterbiLapperConfiguration {
    public int TRACK_LENGTH; // In meters
    public int[] SECTOR_STARTS; // In meters, the final sector ends at TRACK_LENGTH
    public double AVERAGE_RUNNER_SPEED; // In meters per second
    public double DETECTIONS_PER_SECOND; // The number of detections per station, per second
    public double STATION_RANGE_SIGMA; // The sigma parameter of the detection probability of the stations
    double RESTART_PROBABILITY; // The probability that the runners wil start the race in a different spot than the start/finish line (should only happen on complete restarts)

    public ViterbiLapperConfiguration() {
        this.TRACK_LENGTH = 500;
        this.SECTOR_STARTS = new int[]{0, 100, 150, 250, 350};
        this.AVERAGE_RUNNER_SPEED = 3;
        this.DETECTIONS_PER_SECOND = 8;
        this.STATION_RANGE_SIGMA = 50;
        this.RESTART_PROBABILITY = 0.001;
    }
}
