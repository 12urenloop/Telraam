package telraam.database.models;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.Objects;

@Getter @Setter @NoArgsConstructor
public class Baton {
    private Integer id;
    private String name;
    private String mac;

    public Baton(String name) {
        this.name = name;
    }

    public Baton(String name, String mac) {
        this.name = name;
        this.mac = mac;
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
