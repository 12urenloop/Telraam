package telraam.beacon;

/**
 * {@link Callback Callback&lt;Void, Event&lt;B&gt;&gt;} wrapper in disguise.
 * Exposing simpler methods to wrap in the right {@link Event Event&lt;B&gt;}
 * <p>
 * This can be seen as a generic {@link Event Event&lt;B&gt;} sink.
 *
 * @author Arthur Vercruysse
 */
public abstract class EventGenerator<B> {
    protected Callback<Void, Event<B>> handler;

    protected EventGenerator(Callback<Void, Event<B>> handler) {
        this.handler = handler;
    }

    protected void connect() {
        handle(new Event.Connect<>());
    }

    protected void data(B data) {
        handle(new Event.Data<>(data));
    }

    protected void error(Exception e) {
        handle(new Event.Error<>(e));
    }

    protected void exit() {
        handle(new Event.Exit<>());
    }

    protected void handle(Event<B> event) {
        handler.handle(event);
    }
}
