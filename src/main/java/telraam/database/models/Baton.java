package telraam.database.models;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Objects;

public class Baton {
    private Integer id;
    private String name;

    public Baton(ResultSet rs) throws SQLException {
        this.id = rs.getInt("id");
        this.name = rs.getString("name");
    }

    public Baton(String name) {
        this.name = name;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Baton baton = (Baton) o;
        return id.equals(baton.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
