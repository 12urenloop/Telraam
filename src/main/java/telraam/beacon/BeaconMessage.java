package telraam.beacon;

/**
 * BeaconMessage is the representation of what is received from a beacon. This
 * should parse the incoming byte[].
 *
 * @author Arthur Vercruysse
 */
public class BeaconMessage {
    public static final int MESSAGESIZE = Byte.BYTES + Byte.BYTES + Long.BYTES;

    public byte beaconTag;
    public byte batonTag;
    public long timestamp;

    // DO NOT STORE THIS DATA, IT WILL BE OVERWRITTEN
    public BeaconMessage(byte[] data) {
        beaconTag = data[0];
        batonTag = data[1];
        timestamp = ((long) data[9] << 56) | ((long) data[8] & 0xff) << 48 | ((long) data[7] & 0xff) << 40
                | ((long) data[6] & 0xff) << 32 | ((long) data[5] & 0xff) << 24 | ((long) data[4] & 0xff) << 16
                | ((long) data[3] & 0xff) << 8 | ((long) data[2] & 0xff);
    }

    @Override
    public String toString() {
        return String.format("Beacon %o: runner: %o at %d", this.beaconTag, this.batonTag, this.timestamp);
    }
}
