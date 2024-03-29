package telraam.api.responses;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.hibernate.validator.constraints.Length;

@Getter
@NoArgsConstructor
@AllArgsConstructor
public class Saying {
    private long id;

    @Length(max = 3)
    private String content;
}