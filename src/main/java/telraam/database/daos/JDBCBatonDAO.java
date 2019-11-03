package telraam.database.daos;

import telraam.database.ConnectionManager;
import telraam.database.models.Baton;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

public class JDBCBatonDAO implements BatonDAO {
    private static final Logger logger =
            Logger.getLogger(JDBCBatonDAO.class.getName());

    @Override
    public List<Baton> getAll() {
        Connection connection = ConnectionManager.getInstance().getConnection();
        List<Baton> batons = new ArrayList<>();
        try (ResultSet rs = connection.createStatement()
                .executeQuery("select * from baton")) {

            while (rs.next()) {
                batons.add(new Baton(rs.getInt(1), rs.getString(2)));
            }
        } catch (SQLException e) {
            logger.severe(
                    "Failed to retrieve batons \nReason: " + e.getMessage());

        }
        return batons;
    }

    @Override
    public Baton getById(Integer id) {
        return null;
    }

    @Override
    public void insert(Baton newObject) {

    }

    @Override
    public void delete(Integer id) {

    }
}
