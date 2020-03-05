package telraam.database.models;

public class Beacon {
    private Integer id;
    private String name;
    private Integer distance;
    private String macAddress;

    public Beacon() {
    }

    public Beacon(String name) {
        this.name = name;
    }

    public Beacon(String name, Integer distance) {
        this.name = name;
        this.distance = distance;
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

    /**
     * Distance in meters along the track
     *
     * @return
     */
    public Integer getDistance() {
        return distance;
    }

    public void setDistance(Integer distance) {
        this.distance = distance;
    }

    public String getMacAddress() {
        return macAddress;
    }

    public void setMacAddress(String macAddress) {
        this.macAddress = macAddress;
    }
}
