package telraam.database.models;

/**
 * The lap source tells you where the lap comes from.
 */
public class LapSource {
    private Integer id;
    private String name;

    public LapSource() {

    }

    public LapSource(String name) {
        this.name = name;
    }

    public Integer getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public void setName(String name) {
        this.name = name;
    }
}
