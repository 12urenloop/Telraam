package telraam.logic.positioner;

import java.util.LinkedList;

public class CircularQueue<T> extends LinkedList<T> {

    private final int maxSize;
    public CircularQueue(int maxSize) {
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
