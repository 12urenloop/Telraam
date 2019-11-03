package telraam.database.daos;

import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.Config;
import telraam.database.ConnectionManager;
import telraam.database.models.Baton;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.List;
import java.util.logging.Logger;

import static org.junit.jupiter.api.Assertions.*;

class JDBCBatonDAOTest {
    private static final Logger logger =
            Logger.getLogger(JDBCBatonDAOTest.class.getName());
    Connection connection;

    @BeforeEach
    void setUp() throws SQLException {

        this.connection = ConnectionManager.getInstance().getConnection();
        Flyway flyway = Flyway.configure()
                .dataSource(Config.getInstance().getDbUrl(), null, null).load();
        flyway.migrate();
        PreparedStatement statement =
                this.connection.prepareStatement(
                        "insert into baton (name) values (?)");
        statement.setString(1, "baton1");
        statement.execute();
    }

    @Test
    void getAll() {
        BatonDAO dao = new JDBCBatonDAO();
        List<Baton> batons = dao.getAll();
        assertEquals(1, batons.size());
        assertEquals("baton1", batons.get(0).getName());
    }
}