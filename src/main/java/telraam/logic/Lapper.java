package telraam.logic;

import telraam.database.models.Detection;

public interface Lapper {
    void handle(Detection msg);
}
