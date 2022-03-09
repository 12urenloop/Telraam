package telraam.database.daos;

import org.jdbi.v3.core.statement.UnableToExecuteStatementException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.DatabaseTest;
import telraam.database.models.Beacon;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class BeaconDAOTest extends DatabaseTest {

    private BeaconDAO beaconDAO;


    @Override
    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        beaconDAO = jdbi.onDemand(BeaconDAO.class);
    }

    @Test
    void createBeacon() {
        Beacon testBeacon = new Beacon("testbeacon", "localhost:8000");
        final int testId = beaconDAO.insert(testBeacon);
        assertTrue(testId > 0);

        Optional<Beacon> beaconOptional = beaconDAO.getById(testId);
        assertFalse(beaconOptional.isEmpty());
        Beacon beacon = beaconOptional.get();
        assertEquals("testbeacon", beacon.getName());
        assertEquals(false, beacon.getIsBroken());
    }

    @Test
    void testInsertFailsWhenNoName() {
        Beacon testbeacon = new Beacon();
        assertThrows(UnableToExecuteStatementException.class,
                () -> beaconDAO.insert(testbeacon));
    }

    @Test
    void testListBeaconsEmpty() {
        List<Beacon> beacons = beaconDAO.getAll();
        assertNotNull(beacons);
        assertEquals(0, beacons.size());
    }

    @Test
    void testList2Beacons() {
        Beacon b1 = new Beacon("b1", "localhost:8000");
        Beacon b2 = new Beacon("b2", "localhost:8001");
        beaconDAO.insert(b1);
        beaconDAO.insert(b2);

        List<Beacon> beacons = beaconDAO.getAll();
        assertNotNull(beacons);
        assertEquals(2, beacons.size());
        assertNotNull(
                beacons.stream()
                        .filter(beacon -> beacon.getName().equals("b1")));
        assertNotNull(
                beacons.stream()
                        .filter(beacon -> beacon.getName().equals("b2")));
    }

    @Test
    void testFindByIdNullWhenNoBeacon() {
        Optional<Beacon> beaconOptional = beaconDAO.getById(1);
        assertTrue(beaconOptional.isEmpty());
    }

    @Test
    void testUpdateDoesUpdate() {
        Beacon testBeacon = new Beacon("preupdate", "localhost:8000");
        int testid = beaconDAO.insert(testBeacon);
        testBeacon.setId(testid);
        testBeacon.setName("postupdate");
        testBeacon.setBroken(true);
        int updatedRows = beaconDAO.update(testid, testBeacon);
        assertEquals(1, updatedRows);

        Optional<Beacon> dbBeacon = beaconDAO.getById(testid);
        assertFalse(dbBeacon.isEmpty());
        assertEquals("postupdate", dbBeacon.get().getName());
        assertEquals(true, dbBeacon.get().getIsBroken());
    }

    @Test
    void updateDoesntDoAnythingWhenNotExists() {
        Beacon testBeacon = new Beacon("test", "localhost:8000");
        int id = beaconDAO.insert(testBeacon);
        int updatedRows = beaconDAO.update(id + 1, new Beacon("test2", "localhost:8000"));
        List<Beacon> beacons = beaconDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(1, beacons.size());
        assertEquals(testBeacon.getName(), beaconDAO.getById(id).get().getName());
    }

    @Test
    void updateOnlyUpdatesRelevantModel() {
        Beacon testBeacon = new Beacon("test", "localhost:8000");
        int id = beaconDAO.insert(testBeacon);
        Beacon testBeacon2 = new Beacon("test2", "localhost:8000");
        int id2 = beaconDAO.insert(testBeacon2);
        int updatedRows = beaconDAO.update(id, new Beacon("test3", "localhost:8000"));
        List<Beacon> beacons = beaconDAO.getAll();
        assertEquals(1, updatedRows);
        assertEquals(testBeacon2.getName(), beaconDAO.getById(id2).get().getName());
    }

    @Test
    void deleteRemovesBeacon() {
        Beacon testBeacon = new Beacon("test", "localhost:8000");
        int id = beaconDAO.insert(testBeacon);
        int updatedRows = beaconDAO.deleteById(id);

        List<Beacon> beacons = beaconDAO.getAll();
        assertEquals(1, updatedRows);
        assertEquals(0, beacons.size());
    }

    @Test
    void deleteDoesNothingIfNotExists() {
        Beacon testBeacon = new Beacon("test", "localhost:8000");
        int id = beaconDAO.insert(testBeacon);
        int updatedRows = beaconDAO.deleteById(id + 1);

        List<Beacon> beacons = beaconDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(1, beacons.size());
    }
}
