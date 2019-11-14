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
        Beacon testBeacon = new Beacon("testbeacon");
        final int testId = beaconDAO.insert(testBeacon);
        assertTrue(testId > 0);

        Optional<Beacon> beaconOptional = beaconDAO.getById(testId);
        assertFalse(beaconOptional.isEmpty());
        Beacon beacon = beaconOptional.get();
        assertEquals("testbeacon", beacon.getName());
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
        Beacon b1 = new Beacon("b1");
        Beacon b2 = new Beacon("b2");
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
        Beacon testBeacon = new Beacon("preupdate");
        int testid = beaconDAO.insert(testBeacon);
        testBeacon.setId(testid);
        testBeacon.setName("postupdate");
        int updatedRows = beaconDAO.update(testBeacon);
        assertEquals(1, updatedRows);

        Optional<Beacon> dbBeacon = beaconDAO.getById(testid);
        assertFalse(dbBeacon.isEmpty());
        assertEquals("postupdate", dbBeacon.get().getName());
    }

    @Test
    void updateDoesntDoAnythingWhenNotExists() {
        Beacon testBeacon = new Beacon("test");
        int updatedRows = beaconDAO.update(testBeacon);
        List<Beacon> beacons = beaconDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(0, beacons.size());
    }

    @Test
    void deleteRemovesBeacon() {
        Beacon testBeacon = new Beacon("test");
        int id = beaconDAO.insert(testBeacon);
        int updatedRows = beaconDAO.deleteById(id);

        List<Beacon> beacons = beaconDAO.getAll();
        assertEquals(1, updatedRows);
        assertEquals(0, beacons.size());
    }

    @Test
    void deleteDoesNothingIfNotExists() {
        Beacon testBeacon = new Beacon("test");
        int id = beaconDAO.insert(testBeacon);
        int updatedRows = beaconDAO.deleteById(id + 1);

        List<Beacon> beacons = beaconDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(1, beacons.size());
    }
}