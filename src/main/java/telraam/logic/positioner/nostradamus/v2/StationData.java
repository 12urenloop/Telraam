package telraam.logic.positioner.nostradamus.v2;

// Record containing all data regarding a station
public record StationData(
        int distanceToNext, // Meters until the next station
        double progress, // Location of station in progress
        double progressToNext, // Progress until you arrive at the next station
        int nextStationId, // ID of the next station
        int index // Index of the station when sorted by distance from the start
) {}
