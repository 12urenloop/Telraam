package telraam.logic.positioner;

import telraam.database.models.Detection;

public interface Positioner {
    void handle(Detection detection);

}
