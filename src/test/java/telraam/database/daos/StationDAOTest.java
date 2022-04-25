package telraam.database.daos;

import org.jdbi.v3.core.statement.UnableToExecuteStatementException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.DatabaseTest;
import telraam.database.models.Station;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class StationDAOTest extends DatabaseTest {

    private StationDAO stationDAO;


    @Override
    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        stationDAO = jdbi.onDemand(StationDAO.class);
    }

    @Test
    void createStation() {
        Station testStation = new Station("teststation", "localhost:8000");
        final int testId = stationDAO.insert(testStation);
        assertTrue(testId > 0);

        Optional<Station> stationOptional = stationDAO.getById(testId);
        assertFalse(stationOptional.isEmpty());
        Station station = stationOptional.get();
        assertEquals("teststation", station.getName());
        assertEquals(false, station.getIsBroken());
    }

    @Test
    void testInsertFailsWhenNoName() {
        Station teststation = new Station();
        assertThrows(UnableToExecuteStatementException.class,
                () -> stationDAO.insert(teststation));
    }

    @Test
    void testListStationsEmpty() {
        List<Station> stations = stationDAO.getAll();
        assertNotNull(stations);
        assertEquals(0, stations.size());
    }

    @Test
    void testList2Stations() {
        Station b1 = new Station("b1", "localhost:8000");
        Station b2 = new Station("b2", "localhost:8001");
        stationDAO.insert(b1);
        stationDAO.insert(b2);

        List<Station> stations = stationDAO.getAll();
        assertNotNull(stations);
        assertEquals(2, stations.size());
        assertNotNull(
                stations.stream()
                        .filter(station -> station.getName().equals("b1")));
        assertNotNull(
                stations.stream()
                        .filter(station -> station.getName().equals("b2")));
    }

    @Test
    void testFindByIdNullWhenNoStation() {
        Optional<Station> stationOptional = stationDAO.getById(1);
        assertTrue(stationOptional.isEmpty());
    }

    @Test
    void testUpdateDoesUpdate() {
        Station testStation = new Station("preupdate", "localhost:8000");
        int testid = stationDAO.insert(testStation);
        testStation.setId(testid);
        testStation.setName("postupdate");
        testStation.setBroken(true);
        int updatedRows = stationDAO.update(testid, testStation);
        assertEquals(1, updatedRows);

        Optional<Station> dbStation = stationDAO.getById(testid);
        assertFalse(dbStation.isEmpty());
        assertEquals("postupdate", dbStation.get().getName());
        assertEquals(true, dbStation.get().getIsBroken());
    }

    @Test
    void updateDoesntDoAnythingWhenNotExists() {
        Station testStation = new Station("test", "localhost:8000");
        int id = stationDAO.insert(testStation);
        int updatedRows = stationDAO.update(id + 1, new Station("test2", "localhost:8000"));
        List<Station> stations = stationDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(1, stations.size());
        assertEquals(testStation.getName(), stationDAO.getById(id).get().getName());
    }

    @Test
    void updateOnlyUpdatesRelevantModel() {
        Station testStation = new Station("test", "localhost:8000");
        int id = stationDAO.insert(testStation);
        Station testStation2 = new Station("test2", "localhost:8000");
        int id2 = stationDAO.insert(testStation2);
        int updatedRows = stationDAO.update(id, new Station("test3", "localhost:8000"));
        List<Station> stations = stationDAO.getAll();
        assertEquals(1, updatedRows);
        assertEquals(testStation2.getName(), stationDAO.getById(id2).get().getName());
    }

    @Test
    void deleteRemovesStation() {
        Station testStation = new Station("test", "localhost:8000");
        int id = stationDAO.insert(testStation);
        int updatedRows = stationDAO.deleteById(id);

        List<Station> stations = stationDAO.getAll();
        assertEquals(1, updatedRows);
        assertEquals(0, stations.size());
    }

    @Test
    void deleteDoesNothingIfNotExists() {
        Station testStation = new Station("test", "localhost:8000");
        int id = stationDAO.insert(testStation);
        int updatedRows = stationDAO.deleteById(id + 1);

        List<Station> stations = stationDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(1, stations.size());
    }
}
