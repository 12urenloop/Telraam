package telraam.database.models;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * The lap source tells you where the lap comes from.
 */
@Getter @Setter @NoArgsConstructor
public class LapSource {
    private Integer id;
    private String name;

    public LapSource(String name) {
        this.name = name;
    }
}
