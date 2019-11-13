package telraam.database.daos;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.DatabaseTest;

import static org.junit.jupiter.api.Assertions.*;

class BeaconDAOTest extends DatabaseTest {

    private BeaconDAO beaconDAO;

    @Override
    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        beaconDAO = jdbi.onDemand(BeaconDAO.class);
    }

}