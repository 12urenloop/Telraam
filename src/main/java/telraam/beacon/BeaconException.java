package telraam.beacon;

public class BeaconException extends Exception {
    protected BeaconException(String reason) {
        super(reason);
    }

    public static class MsgEndWithNoStart extends BeaconException {
        public MsgEndWithNoStart() {
            super("Message end tag detected without a start tag");
        }
    }

    public static class MsgStartWithNoEnd extends BeaconException {
        public MsgStartWithNoEnd() {
            super("2 message start tags detected.");
        }
    }
}
