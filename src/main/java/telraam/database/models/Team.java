package telraam.database.models;

public class Team {
    private Integer id;
    private String name;

    public Team() {
    }

    public Team(String name) {
        this.name = name;
    }

    public Team(String name, int batonId) {
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

}
