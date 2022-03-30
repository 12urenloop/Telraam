package telraam.logic.viterbi;

public class ViterbiLapperConfiguration {
    public double RESTART_PROBABILITY;      // The probability that the runners will start the race in a different spot than the start/finish line (should only happen on complete restarts)
    public int DEBOUNCE_TIMEOUT;            // The amount of time detections are debounced for in seconds

    // probability that you will be  for detection in the segment corresponding to a station
    public double SAME_STATION_DETECTION_CHANCE;

    // the probability that you will be detected at a random station ("noise" detections)
    public double BASE_DETECTION_CHANCE;

    // the probability that an individual station is down at any moment in time
    // ~= downtime / total time.
    // It is not that important that this parameter is estimated correctly (which would be very hard to do)
    // it is better to interpret this as the system's eagerness to assume that a station was not working
    // when trying to explain a series of detections.
    public double BROKEN_STATION_PROBABILITY;

    // The amount of times we expect a runner to be detected when passing by a station, given that the station is alive.
    //
    // The higher this parameter, the more 'evidence' (= detections) the system will require
    // in order to change the predicted location - so, the predictions will be less sensitive to noise detections
    // at different stations.
    // Consequently, it is better to under-estimate this parameter than to over-estimate it.
    public double EXPECTED_NUM_DETECTIONS;

    public ViterbiLapperConfiguration() {
        this.RESTART_PROBABILITY = 0.001;
        this.DEBOUNCE_TIMEOUT = 10;

        // ballpark estimates extracted from test event data
        // (ask @iasoon for details)
        this.SAME_STATION_DETECTION_CHANCE = 0.5;
        this.BASE_DETECTION_CHANCE = 0.125;
        this.EXPECTED_NUM_DETECTIONS = 80;


        this.BROKEN_STATION_PROBABILITY = 0.01;
    }
}
