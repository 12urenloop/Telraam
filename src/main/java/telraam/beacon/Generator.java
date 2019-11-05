package telraam.beacon;

public abstract class Generator<T> {
    protected Callback<Void, T> handler;

    public Generator(Callback<Void, T> handler) {
        this.handler = handler;
    }

    protected void handle(T element) {
        handler.handle(element);
    }
}
