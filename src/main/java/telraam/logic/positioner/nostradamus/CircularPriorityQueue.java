package telraam.logic.positioner.nostradamus;

import telraam.database.models.Detection;

import java.util.Comparator;
import java.util.PriorityQueue;

public class CircularPriorityQueue extends PriorityQueue<Detection> {
    private final int maxSize;

    // TODO: Check if in the right order. Oldest timestamp (the earliest) should be first
    public CircularPriorityQueue(int maxSize) {
        super(Comparator.comparing(Detection::getTimestamp));

        this.maxSize = maxSize;
    }

    @Override
    public boolean add(Detection e) {
        if (size() >= this.maxSize) {
            if (e.getTimestamp().after(peek().getTimestamp())) {
                remove();
            }
        }

        return super.add(e);
    }
}
