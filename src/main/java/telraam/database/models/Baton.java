package telraam.database.models;

import java.util.Objects;

public class Baton {
    private Integer id;
    private String name;
    private String mac;

    // DO NOT REMOVE
    public Baton(){}

    public Baton(String name, String mac) {
        this.name = name;
        this.mac = mac;
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

    public String getMac() {
        return mac;
    }

    public void setMac(String mac) {
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
