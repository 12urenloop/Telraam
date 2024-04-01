package telraam.station;

import org.jdbi.v3.core.Jdbi;
import telraam.database.models.Station;
import telraam.logic.lapper.Lapper;
import telraam.logic.positioner.Positioner;
import telraam.station.http.HTTPFetcher;
import telraam.station.websocket.WebsocketFetcher;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Set;
import java.util.logging.Logger;

public class FetcherFactory {
    private final Logger logger = Logger.getLogger(FetcherFactory.class.getName());
    private final Jdbi database;
    private final Set<Lapper> lappers;
    private final Set<Positioner> positioners;
    public FetcherFactory(Jdbi database, Set<Lapper> lappers, Set<Positioner> positioners) {
        this.database = database;
        this.lappers = lappers;
        this.positioners = positioners;
    }

    public Fetcher create(Station station) {
        try {
            URI stationURI = new URI(station.getUrl());
            return switch (stationURI.getScheme()) {
                case "ws" -> new WebsocketFetcher(database, station, lappers, positioners);
                case "http" -> new HTTPFetcher(database, station, lappers, positioners);
                default -> {
                    logger.severe(String.format("%s is not a valid scheme for a station", stationURI.getScheme()));
                    yield null;
                }
            };
        } catch (URISyntaxException e) {
            logger.severe(String.format("Failed to parse station URI: %s", e.getMessage()));
        }
        return null;
    }
}
