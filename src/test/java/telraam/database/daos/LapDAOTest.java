package telraam.database.daos;

import org.jdbi.v3.core.statement.UnableToExecuteStatementException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.DatabaseTest;
import telraam.database.models.Lap;
import telraam.database.models.LapSource;
import telraam.database.models.Team;

import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class LapDAOTest extends DatabaseTest {
    private final Timestamp exampleTime = new Timestamp(123456789);
    private LapDAO lapDAO;
    private TeamDAO teamDAO;
    private Team exampleTeam;
    private LapSourceDAO lapSourceDAO;
    private LapSource exampleSource;

    @Override
    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        lapDAO = jdbi.onDemand(LapDAO.class);
        teamDAO = jdbi.onDemand(TeamDAO.class);
        lapSourceDAO = jdbi.onDemand(LapSourceDAO.class);
        exampleTeam = new Team("exampleTeam");
        int teamId = teamDAO.insert(exampleTeam);
        exampleTeam.setId(teamId);
        exampleSource = new LapSource("example");
        int sourceId = lapSourceDAO.insert(exampleSource);
        exampleSource.setId(sourceId);
    }

    @Test
    void createLap() {

        Lap testlap = new Lap(exampleTeam.getId(), exampleSource.getId(), exampleTime);
        final int testId = lapDAO.insert(testlap);
        assertTrue(testId > 0);

        Optional<Lap> lapOptional = lapDAO.getById(testId);
        assertFalse(lapOptional.isEmpty());
        Lap lap = lapOptional.get();
        assertEquals(exampleTime, lap.getTimestamp());
    }

    @Test
    void testInsertFailsWhenNoTeam() {
        Lap testlap = new Lap();
        assertThrows(UnableToExecuteStatementException.class, () -> lapDAO.insert(testlap));
    }

    @Test
    void testListLapsEmpty() {
        List<Lap> laps = lapDAO.getAll();
        assertNotNull(laps);
        assertEquals(0, laps.size());
    }

    @Test
    void testList2Laps() {
        Lap b1 = new Lap(exampleTeam.getId(), exampleSource.getId(), exampleTime);
        Lap b2 = new Lap(exampleTeam.getId(), exampleSource.getId(), exampleTime);
        lapDAO.insert(b1);
        lapDAO.insert(b2);

        List<Lap> laps = lapDAO.getAll();
        assertNotNull(laps);
        assertEquals(2, laps.size());
        assertNotNull(
                laps.stream()
                        .filter(lap -> lap.getTimestamp().equals(exampleTime)));
    }

    @Test
    void testFindByIdNullWhenNoLap() {
        Optional<Lap> lapOptional = lapDAO.getById(1);
        assertTrue(lapOptional.isEmpty());
    }

    @Test
    void testUpdateDoesUpdate() {
        Lap testLap = new Lap(exampleTeam.getId(), exampleSource.getId(), exampleTime);
        int testid = lapDAO.insert(testLap);
        testLap.setId(testid);
        Timestamp updated = new Timestamp(987654321);
        testLap.setTimestamp(updated);
        int updatedRows = lapDAO.update(testid, testLap);
        assertEquals(1, updatedRows);

        Optional<Lap> dbLap = lapDAO.getById(testid);
        assertFalse(dbLap.isEmpty());
        assertEquals(updated, dbLap.get().getTimestamp());
    }

    @Test
    void updateDoesntDoAnythingWhenNotExists() {
        Lap testLap = new Lap(exampleTeam.getId(), exampleSource.getId(), exampleTime);
        int id = lapDAO.insert(testLap);
        int updatedRows = lapDAO.update(id + 1, testLap);
        List<Lap> laps = lapDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(1, laps.size());
    }

    @Test
    void updateOnlyUpdatesRelevantModel() {
        Lap testLap = new Lap(exampleTeam.getId(), exampleSource.getId(), exampleTime);
        int id = lapDAO.insert(testLap);
        Lap testLap2 = new Lap(exampleTeam.getId(), exampleSource.getId(), new Timestamp(123456790));
        int id2 = lapDAO.insert(testLap2);
        int updatedRows = lapDAO.update(id, new Lap(exampleTeam.getId(), exampleSource.getId(), new Timestamp(123456791)));
        assertEquals(1, updatedRows);
        assertEquals(testLap2.getTimestamp(), lapDAO.getById(id2).get().getTimestamp());
    }

    @Test
    void deleteRemovesLap() {
        Lap testLap = new Lap(exampleTeam.getId(), exampleSource.getId(), exampleTime);
        int id = lapDAO.insert(testLap);
        int updatedRows = lapDAO.deleteById(id);

        List<Lap> laps = lapDAO.getAll();
        assertEquals(1, updatedRows);
        assertEquals(0, laps.size());
    }

    @Test
    void deleteDoesNothingIfNotExists() {
        Lap testLap = new Lap(exampleTeam.getId(), exampleSource.getId(), exampleTime);
        int id = lapDAO.insert(testLap);
        int updatedRows = lapDAO.deleteById(id + 1);

        List<Lap> laps = lapDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(1, laps.size());
    }
}
