package telraam.database.models;

public class Beacon {
    private Integer id;
    private String name;
    private String mac;

    public Beacon() {}

    public Beacon(String name, String mac) {
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
}
