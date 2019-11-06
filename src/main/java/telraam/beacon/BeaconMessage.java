package telraam.beacon;

public class BeaconMessage {
    public static final int MESSAGESIZE = 10;

    public byte[] data;

    public BeaconMessage() {
    }

    // DO NOT STORE THIS DATA, IT WILL BE OVERWRITTEN
    public BeaconMessage(byte[] data) {
        this.data = data;
    }
}
