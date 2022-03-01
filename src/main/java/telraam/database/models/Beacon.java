package telraam.database.models;

public class Beacon {
    private Integer id;
    private String name;
    private Boolean isBroken;

    public Beacon() {
      this.isBroken = false;
    }

    public Beacon(String name) {
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

    public Boolean getIsBroken() {
        return isBroken;
    }

    public void setBroken(Boolean isBroken) { this.isBroken = isBroken; }
}
