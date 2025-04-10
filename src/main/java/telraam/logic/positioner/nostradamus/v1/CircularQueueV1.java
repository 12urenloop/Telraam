package telraam.logic.positioner.nostradamus.v1;

import java.util.LinkedList;

// LinkedList with a maximum length
public class CircularQueueV1<T> extends LinkedList<T> {

    private final int maxSize;
    public CircularQueueV1(int maxSize) {
        this.maxSize = maxSize;
    }

    @Override
    public boolean add(T e) {
        if (size() >= this.maxSize) {
            removeFirst();
        }

        return super.add(e);
    }

}
