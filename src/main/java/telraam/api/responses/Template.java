package telraam.api.responses;

import lombok.AllArgsConstructor;

import java.util.Optional;

import static java.lang.String.format;

@AllArgsConstructor
public class Template {
    private final String content;
    private final String defaultName;

    public String render(Optional<String> name) {
        return format(content, name.orElse(defaultName));
    }
}