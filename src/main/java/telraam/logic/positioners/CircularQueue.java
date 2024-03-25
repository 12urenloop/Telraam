package telraam.logic.positioners;

import java.util.LinkedList;

public class CircularQueue<T> extends LinkedList<T> {

    private final int maxSize;
    private int size = 0;

    public CircularQueue(int maxSize) {
        this.maxSize = maxSize;
    }

    @Override
    public boolean add(T e) {
        if (this.size >= this.maxSize) {
            super.removeFirst();
            this.size--;
        }

        boolean result = super.add(e);

        if (result) {
            this.size++;
        }

        return result;
    }

}
