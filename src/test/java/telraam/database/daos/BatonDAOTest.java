package telraam.database.daos;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.DatabaseTest;
import telraam.database.models.Baton;
import telraam.database.models.Id;

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
        batonDAO = jdbi.onDemand(BatonDAO.class);
        Baton testbaton = new Baton("testbaton");
        final Id testId = batonDAO.insert(testbaton);
        assertTrue(testId.getId() > 0);

        Optional<Baton> batonOptional = batonDAO.findBatonById(testId.getId());
        assertFalse(batonOptional.isEmpty());
        Baton baton = batonOptional.get();
        assertEquals("testbaton", baton.getName());

    }
}

