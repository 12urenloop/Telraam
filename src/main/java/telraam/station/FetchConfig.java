package telraam.station;

public record FetchConfig(WaitBetween waitPolicy, int waitMs) {
    public enum WaitBetween {
        // Wait between fetch each station individually
        PER_STATION, 

        // Only wait after fetching all stations at the same time
        PER_STATION_BLOCK
    }
}
