package telraam.beacon;

public abstract class EventGenerator<B> extends Generator<Event<B>> {
    public EventGenerator(Callback<Void, Event<B>> handler) {
        super(handler);
    }

    protected void connect() {
        super.handle(new Event.Connect<>());
    }

    protected void data(B data) {
        super.handle(new Event.Data<>(data));
    }

    protected void error(Exception e) {
        super.handle(new Event.Error<>(e));
    }

    protected void exit() {
        super.handle(new Event.Exit<>());
    }
}
