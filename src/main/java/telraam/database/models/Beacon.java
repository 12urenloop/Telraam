package telraam.database.models;

public class Beacon {
    private Integer id;
    private String name;
    private Double distanceFromStart;
    private Boolean isBroken;
    private String url;

    public Beacon() {
      this.isBroken = false;
    }

    public Beacon(String name, String url) {
        this.name = name;
        this.isBroken = false;
        this.url = url;
    }
    public Beacon(String name, boolean isBroken) {
        this.name = name;
        this.isBroken = isBroken;
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

    public Double getDistanceFromStart() {
        return distanceFromStart;
    }

    public void setDistanceFromStart(Double distanceFromStart) {
        this.distanceFromStart = distanceFromStart;
    }

    public Boolean getIsBroken() {
        return isBroken;
    }

    public void setBroken(Boolean isBroken) { this.isBroken = isBroken; }

    public String getUrl() {
        return this.url;
    }
}
