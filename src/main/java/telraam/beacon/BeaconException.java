package telraam.beacon;

public class BeaconException extends Exception {
    private static final long serialVersionUID = 4538291;
    protected BeaconException(String reason) {
        super(reason);
    }

    public static class MsgEndWithNoStart extends BeaconException {
        private static final long serialVersionUID = 6893402;
        public MsgEndWithNoStart() {
            super("Message end tag detected without a start tag");
        }
    }

    public static class MsgStartWithNoEnd extends BeaconException {
        private static final long serialVersionUID = 5902819;
        public MsgStartWithNoEnd() {
            super("2 message start tags detected.");
        }
    }

    public static class MsgToShort extends BeaconException {
        private static final long serialVersionUID = 3019424;
        public MsgToShort(int expected, int actual) {
            super("Cannot parse message with size " + actual + " expected " + expected+".");
        }
    }
}
