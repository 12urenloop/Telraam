package telraam.database.models;

import java.security.Principal;

public record User(String username) implements Principal {
    @Override
    public String getName() {
        return username;
    }
}
