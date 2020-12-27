package telraam.beacon;

/**
* Event is an 'enum' class with data attached.
* An events can be handled with an EventHandler like a TCPFactory.
* `event.handle(this);`
*
* @author  Arthur Vercruysse
*/
public abstract class Event<B> {

    abstract void handle(EventHandler<B> h);

    public static class Data<B> extends Event<B> {
        private B inner;

        public Data(B data) {
            setInner(data);
        }

        void handle(EventHandler<B> h) {
            h.data(getInner());
        }

        public B getInner() {
            return inner;
        }

        public void setInner(B inner) {
            this.inner = inner;
        }
    }

    public static class Error<B> extends Event<B> {
        private Exception inner;

        public Error(Exception e) {
            setInner(e);
        }

        void handle(EventHandler<B> h) {
            h.error(getInner());
        }

        public Exception getInner() {
            return inner;
        }

        public void setInner(Exception inner) {
            this.inner = inner;
        }
    }

    public static class Connect<B> extends Event<B> {
        public Connect() {
        }

        void handle(EventHandler<B> h) {
            h.connect();
        }
    }

    public static class Exit<B> extends Event<B> {
        public Exit() {
        }

        void handle(EventHandler<B> h) {
            h.exit();
        }
    }

    public interface EventHandler<B> {
        void exit();
        void connect();

        void error(Exception e);

        void data(B b);
    }
}
