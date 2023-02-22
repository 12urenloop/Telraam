package telraam.database.daos;

import org.jdbi.v3.core.statement.UnableToExecuteStatementException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.DatabaseTest;
import telraam.database.models.Baton;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class BatonDAOTest extends DatabaseTest {
    private BatonDAO batonDAO;

    @Override
    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        batonDAO = jdbi.onDemand(BatonDAO.class);
    }

    @Test
    void createBaton() {
        Baton testbaton = new Baton("testbaton", "mac1");
        final int testId = batonDAO.insert(testbaton);
        assertTrue(testId > 0);

        Optional<Baton> batonOptional = batonDAO.getById(testId);
        assertFalse(batonOptional.isEmpty());
        Baton baton = batonOptional.get();
        assertEquals("testbaton", baton.getName());
        assertEquals("mac1", baton.getMac());
    }

    @Test
    void testInsertFailsWhenNoName() {
        Baton testbaton = new Baton();
        assertThrows(UnableToExecuteStatementException.class, () -> batonDAO.insert(testbaton));
    }

    @Test
    void testListBatonsEmpty() {
        List<Baton> batons = batonDAO.getAll();
        assertNotNull(batons);
        assertEquals(0, batons.size());
    }

    @Test
    void testList2Batons() {
        Baton b1 = new Baton("b1", "mac1");
        Baton b2 = new Baton("b2", "mac2");
        batonDAO.insert(b1);
        batonDAO.insert(b2);

        List<Baton> batons = batonDAO.getAll();
        assertNotNull(batons);
        assertEquals(2, batons.size());
        assertNotNull(
                batons.stream().filter(baton -> baton.getName().equals("b1") && baton.getMac().equals("mac1")));
        assertNotNull(
                batons.stream().filter(baton -> baton.getName().equals("b2") && baton.getMac().equals("mac2")));
    }

    @Test
    void testFindByIdNullWhenNoBaton() {
        Optional<Baton> batonOptional = batonDAO.getById(1);
        assertTrue(batonOptional.isEmpty());
    }

    @Test
    void testUpdateDoesUpdate() {
        Baton testBaton = new Baton("namePreUpdate", "macPreUpdate");
        int testid = batonDAO.insert(testBaton);
        testBaton.setId(testid);
        testBaton.setName("namePostUpdate");
        testBaton.setMac("macPostUpdate");
        int updatedRows = batonDAO.update(testid, testBaton);
        assertEquals(1, updatedRows);

        Optional<Baton> dbBaton = batonDAO.getById(testid);
        assertFalse(dbBaton.isEmpty());
        assertEquals("namePostUpdate", dbBaton.get().getName());
        assertEquals("macPostUpdate", dbBaton.get().getMac());
    }

    @Test
    void updateDoesntDoAnythingWhenNotExists() {
        Baton testBaton = new Baton("test", "mac1");
        int id = batonDAO.insert(testBaton);
        testBaton.setId(id);
        int updatedRows = batonDAO.update(id + 1, new Baton("test2", "mac2"));
        List<Baton> batons = batonDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(1, batons.size());
        assertEquals(testBaton, batonDAO.getById(id).get());
    }

    @Test
    void deleteRemovesBaton() {
        Baton testBaton = new Baton("test", "mac1");
        int id = batonDAO.insert(testBaton);
        int updatedRows = batonDAO.deleteById(id);

        List<Baton> batons = batonDAO.getAll();
        assertEquals(1, updatedRows);
        assertEquals(0, batons.size());
    }

    @Test
    void deleteDoesNothingIfNotExists() {
        Baton testBaton = new Baton("test", "mac1");
        int id = batonDAO.insert(testBaton);
        int updatedRows = batonDAO.deleteById(id + 1);

        List<Baton> batons = batonDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(1, batons.size());
    }
}

