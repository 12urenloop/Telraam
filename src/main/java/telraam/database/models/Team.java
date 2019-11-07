package telraam.database.models;

public class Team {
    private Integer id;
    private String name;
    private Integer baton_id;

    public Team() {};

    public Team(String name) {
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

    public Integer getBaton_id() {
        return baton_id;
    }

    public void setBaton_id(Integer baton_id) {
        this.baton_id = baton_id;
    }
}
