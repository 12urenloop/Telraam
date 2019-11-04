package telraam.database.daos;

import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.Config;
import telraam.database.DataAccessContext;
import telraam.database.DataAccessException;
import telraam.database.Database;
import telraam.database.models.Baton;

import java.util.List;
import java.util.logging.Logger;

import static org.junit.jupiter.api.Assertions.assertEquals;

class JDBCBatonDAOTest {
    private static final Logger logger =
            Logger.getLogger(JDBCBatonDAOTest.class.getName());

    // We need to use one context during testing. The tables are dropped when the connection is closed.
    private DataAccessContext dac;

    @BeforeEach
    void setUp() {
        dac =  Database.getInstance().getDataAccessContext();
        Flyway flyway = Flyway.configure()
                .dataSource(Config.getInstance().getDbUrl(), null, null).load();
        flyway.migrate();
    }

    @AfterEach
    void BreakDown() throws DataAccessException {
        dac.close();
    }

    @Test
    void insert(){
        Baton newBaton = dac.getBatonDAO().insert(new Baton("baton2"));

        assertEquals(1, newBaton.getId()); // It's the first generated id in the table
        assertEquals("baton2", newBaton.getName());
    }

    @Test
    void getAll() {
        dac.getBatonDAO().insert(new Baton("baton1"));

        List<Baton> batons = dac.getBatonDAO().getAll();
        assertEquals(1, batons.size());
        assertEquals("baton1", batons.get(0).getName());
    }
}