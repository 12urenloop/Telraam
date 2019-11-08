package telraam.beacon;

/**
* Callback<Void, Event<B>> wrapper in disguise.
* Exposing simpler methods to wrap in the right Event<B>.
*
* @author  Arthur Vercruysse
*/
public abstract class EventGenerator<B> {
    protected Callback<Void, Event<B>> handler;

    public EventGenerator(Callback<Void, Event<B>> handler) {
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