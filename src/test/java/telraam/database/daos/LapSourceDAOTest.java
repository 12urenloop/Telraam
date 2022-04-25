package telraam.database.daos;

import org.jdbi.v3.core.statement.UnableToExecuteStatementException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.DatabaseTest;
import telraam.database.models.LapSource;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class LapSourceDAOTest extends DatabaseTest {

    private LapSourceDAO lapSourceDAO;
    private long existing;

    @Override
    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        lapSourceDAO = jdbi.onDemand(LapSourceDAO.class);
        existing = lapSourceDAO.count();
    }

    @Test
    void createLapSource() {
        LapSource lapSource = new LapSource("test");
        final int testId = lapSourceDAO.insert(lapSource);
        assertTrue(testId > 0);

        Optional<LapSource> lapSourceOptional = lapSourceDAO.getById(testId);
        assertFalse(lapSourceOptional.isEmpty());
        LapSource fromDatabase = lapSourceOptional.get();
        assertEquals("test", fromDatabase.getName());
    }

    @Test
    void testInsertFailsWhenNoName() {
        LapSource lapSource = new LapSource();
        assertThrows(UnableToExecuteStatementException.class,
                () -> lapSourceDAO.insert(lapSource));

    }

    @Test
    void testListLapSourcesEmpty() {
        List<LapSource> teams = lapSourceDAO.getAll();
        assertNotNull(teams);
        assertEquals(existing, teams.size());
    }

    @Test
    void testList2LapSources() {
        LapSource b1 = new LapSource("b1");
        LapSource b2 = new LapSource("b2");
        lapSourceDAO.insert(b1);
        lapSourceDAO.insert(b2);

        List<LapSource> lapSources = lapSourceDAO.getAll();
        assertNotNull(lapSources);
        assertEquals(existing + 2, lapSources.size());
        assertNotNull(
                lapSources.stream().filter(source -> source.getName().equals("b1")));
        assertNotNull(
                lapSources.stream().filter(source -> source.getName().equals("b2")));
    }

    @Test
    void testFindByIdNullWhenNoLapSource() {
        Optional<LapSource> lapSourceOptional = lapSourceDAO.getById(500);
        assertTrue(lapSourceOptional.isEmpty());
    }

    @Test
    void testFindByNameNullWhenNoLapSource() {
        Optional<LapSource> lapSourceOptional = lapSourceDAO.getByName("test");
        assertTrue(lapSourceOptional.isEmpty());
    }

    @Test
    void testFindByNameWorks() {
        LapSource source = new LapSource("test-source");
        lapSourceDAO.insert(source);
        Optional<LapSource> lapSourceOptional = lapSourceDAO.getByName("test-source");
        assertFalse(lapSourceOptional.isEmpty());
        assertEquals("test-source", lapSourceOptional.get().getName());
    }

    @Test
    void testUpdateDoesUpdate() {
        LapSource lapSource = new LapSource("preupdate");
        int testId = lapSourceDAO.insert(lapSource);
        lapSource.setId(testId);
        lapSource.setName("postupdate");
        int updatedRows = lapSourceDAO.update(testId, lapSource);
        assertEquals(1, updatedRows);

        Optional<LapSource> dbLapSource = lapSourceDAO.getById(testId);
        assertFalse(dbLapSource.isEmpty());
        assertEquals("postupdate", dbLapSource.get().getName());
    }

    @Test
    void updateDoesntDoAnythingWhenNotExists() {
        LapSource testLapSource = new LapSource("test");
        int id = lapSourceDAO.insert(testLapSource);
        int updatedRows = lapSourceDAO.update(id + 1, testLapSource);
        List<LapSource> lapSources = lapSourceDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(existing + 1, lapSources.size());
    }

    @Test
    void updateOnlyUpdatesRelevantModel() {
        LapSource testLapSource = new LapSource("test");
        int id = lapSourceDAO.insert(testLapSource);
        LapSource testLapSource2 = new LapSource("test2");
        int id2 = lapSourceDAO.insert(testLapSource2);
        LapSource updateSource = new LapSource("updated");
        int updatedRows = lapSourceDAO.update(id, updateSource);
        List<LapSource> lapSources = lapSourceDAO.getAll();
        assertEquals(1, updatedRows);
        assertEquals(testLapSource2.getName(), lapSourceDAO.getById(id2).get().getName());
        assertEquals(existing + 2, lapSources.size());
    }

    @Test
    void deleteRemovesLapSource() {
        LapSource lapSource = new LapSource("test");
        int id = lapSourceDAO.insert(lapSource);
        int updatedRows = lapSourceDAO.deleteById(id);

        List<LapSource> lapSources = lapSourceDAO.getAll();
        assertEquals(1, updatedRows);
        assertEquals(existing, lapSources.size());
    }

    @Test
    void deleteDoesNothingIfNotExists() {
        LapSource lapSource = new LapSource("test");
        int id = lapSourceDAO.insert(lapSource);
        int updatedRows = lapSourceDAO.deleteById(id + 1);

        List<LapSource> lapSources = lapSourceDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(existing + 1, lapSources.size());
    }
}
