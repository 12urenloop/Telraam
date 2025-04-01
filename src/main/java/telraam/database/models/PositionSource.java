package telraam.database.models;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class PositionSource {
  private Integer id;
  private String name;

  public PositionSource(String name) {
    this.name = name;
  }
}
