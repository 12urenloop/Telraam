package telraam.database.models;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter
public class Station {
    private Integer id;
    private String name;
    private Double distanceFromStart;
    @Getter @Setter
    private Boolean broken;
    private String url;
    private Double coordX;
    private Double coordY;

    public Station() {
        this.broken = false;
    }

    public Station(String name, String url) {
        this.name = name;
        this.broken = false;
        this.url = url;
    }

    public Station(String name, Double distanceFromStart, String url) {
        this.name = name;
        this.broken = false;
        this.distanceFromStart = distanceFromStart;
        this.url = url;
    }

    public Station(String name, boolean isBroken) {
        this.name = name;
        this.broken = isBroken;
    }
}
