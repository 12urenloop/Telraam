package telraam.database.models;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter
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

    // These are overridden, otherwise the updateDoesUpdate test fails for some reason?
    public void setIsBroken(boolean isBroken) {
        this.isBroken = isBroken;
    }
    public void setBroken(boolean isBroken) {
        this.isBroken = isBroken;
    }
}
