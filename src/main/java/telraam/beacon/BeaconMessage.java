package telraam.beacon;

public class BeaconMessage {
    public byte[] data;

    public BeaconMessage() {
    }

    // DO NOT STORE THIS DATA, IT WILL BE OVERWRITTEN
    public BeaconMessage(byte[] data) {
        this.data = data;
    }
}
