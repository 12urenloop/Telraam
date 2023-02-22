package telraam.database.daos;

import org.jdbi.v3.core.statement.UnableToExecuteStatementException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.DatabaseTest;
import telraam.database.models.Baton;
import telraam.database.models.Station;
import telraam.database.models.Detection;

import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

class DetectionDAOTest extends DatabaseTest {

    private DetectionDAO detectionDAO;
    private int batonId1;
    private int stationId;
    private Detection exampleDetection;


    @Override
    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        detectionDAO = jdbi.onDemand(DetectionDAO.class);
        BatonDAO batonDAO = jdbi.onDemand(BatonDAO.class);
        StationDAO stationDAO = jdbi.onDemand(StationDAO.class);
        batonId1 = batonDAO.insert(new Baton("baton1", "mac1"));
        stationId = stationDAO.insert(new Station("station1", 1d, "localhost:8000"));
        exampleDetection =
                new Detection(batonId1, stationId, -80, 100.0f, 1L, 1, new Timestamp(1));
    }

    @Test
    void createDetection() {
        final int testId = detectionDAO.insert(exampleDetection);
        assertTrue(testId > 0);

        Optional<Detection> detectionOptional = detectionDAO.getById(testId);
        assertFalse(detectionOptional.isEmpty());
        Detection detection = detectionOptional.get();
        assertEquals(new Timestamp(1), detection.getTimestamp());
    }

    @Test
    void testInsertFailsWhenNoTimestamp() {
        Detection testdetection = new Detection();
        assertThrows(UnableToExecuteStatementException.class,
                () -> detectionDAO.insert(testdetection));
    }

    @Test
    void testListDetectionsEmpty() {
        List<Detection> detections = detectionDAO.getAll();
        assertNotNull(detections);
        assertEquals(0, detections.size());
    }

    @Test
    void testList2Detections() {
        Detection b1 = exampleDetection;
        Detection b2 = exampleDetection;
        b2.setTimestamp(new Timestamp(2));
        detectionDAO.insert(b1);
        detectionDAO.insert(b2);

        List<Detection> detections = detectionDAO.getAll();
        assertNotNull(detections);
        assertEquals(2, detections.size());
    }

    @Test
    void testFindByIdNullWhenNoDetection() {
        Optional<Detection> detectionOptional = detectionDAO.getById(1);
        assertTrue(detectionOptional.isEmpty());
    }

    @Test
    void testUpdateDoesUpdate() {
        int testid = detectionDAO.insert(exampleDetection);
        Timestamp afterTime = new Timestamp(1);
        exampleDetection.setTimestamp(afterTime);

        int updatedRows = detectionDAO.update(testid, exampleDetection);
        assertEquals(1, updatedRows);

        Optional<Detection> dbDetection = detectionDAO.getById(testid);
        assertFalse(dbDetection.isEmpty());
        assertEquals(afterTime, dbDetection.get().getTimestamp());
    }

    @Test
    void updateDoesntDoAnythingWhenNotExists() {
        int testid = detectionDAO.insert(exampleDetection);
        exampleDetection.setId(testid);
        int updatedRows = detectionDAO.update(testid + 1, new Detection(batonId1, stationId, -90, 100.0f, 1L, 1, new Timestamp(1)));
        List<Detection> detections = detectionDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(1, detections.size());
        assertEquals(exampleDetection.getTimestamp(), detectionDAO.getById(testid).get().getTimestamp());
    }

    @Test
    void updateOnlyUpdatesRelevantModel() {
        int id1 = detectionDAO.insert(exampleDetection);
        Detection detection2 = new Detection(batonId1, stationId, -80, 100.0f, 1L, 1, new Timestamp(1));
        int id2 = detectionDAO.insert(detection2);
        int updatedRows = detectionDAO.update(id1, new Detection(batonId1, stationId, -80, 100.0f, 2L, 2, new Timestamp(2)));
        assertEquals(1, updatedRows);
        assertEquals(2, detectionDAO.getAll().size());
        assertEquals(detection2.getTimestamp(), detectionDAO.getById(id2).get().getTimestamp());
    }

    @Test
    void deleteRemovesDetection() {
        int id = detectionDAO.insert(exampleDetection);
        int updatedRows = detectionDAO.deleteById(id);

        List<Detection> detections = detectionDAO.getAll();
        assertEquals(1, updatedRows);
        assertEquals(0, detections.size());
    }

    @Test
    void deleteDoesNothingIfNotExists() {
        int id = detectionDAO.insert(exampleDetection);
        int updatedRows = detectionDAO.deleteById(id + 1);

        List<Detection> detections = detectionDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(1, detections.size());
    }
}
