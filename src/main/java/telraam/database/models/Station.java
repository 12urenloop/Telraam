package telraam.database.models;

public class Station {
    private Integer id;
    private String name;
    private Double distanceFromStart;
    private Boolean isBroken;
    private String url;
    private Double coordX;
    private Double coordY;

    public Station() {
        this.isBroken = false;
    }

    public Station(String name, String url) {
        this.name = name;
        this.isBroken = false;
        this.url = url;
    }

    public Station(String name, Double distanceFromStart, String url) {
        this.name = name;
        this.isBroken = false;
        this.distanceFromStart = distanceFromStart;
        this.url = url;
    }

    public Station(String name, boolean isBroken) {
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

    public void setBroken(Boolean isBroken) {
        this.isBroken = isBroken;
    }

    public String getUrl() {
        return this.url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public Double getCoordX() {
        return this.coordX;
    }

    ;

    public void setCoordX(Double coordX) {
        this.coordX = coordX;
    }

    public Double getCoordY() {
        return this.coordY;
    }

    public void setCoordY(Double coordY) {
        this.coordY = coordY;
    }
}
