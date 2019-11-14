package telraam.beacon;

import telraam.database.models.Detection;

import java.sql.Timestamp;
import java.util.List;

/**
 * BeaconMessage is the representation of what is received from a beacon. This
 * should parse the incoming byte.get().
 *
 * @author Arthur Vercruysse
 */
public class BeaconMessage {
    public static final int MESSAGESIZE = Byte.BYTES + Byte.BYTES + Long.BYTES;

    public static final byte[] STARTTAG = {'<', '<', '<', '<'};
    public static final byte[] ENDTAG = {'>', '>', '>', '>'};

    public byte beaconTag;
    public byte batonTag;
    public long timestamp;

    // DO NOT STORE THIS DATA, IT WILL BE OVERWRITTEN
    public BeaconMessage(List<Byte> data) {
        beaconTag = data.get(0);
        batonTag = data.get(1);
        timestamp =
                ((long) data.get(9) << 56) | ((long) data.get(8) & 0xff) << 48 |
                        ((long) data.get(7) & 0xff) << 40
                        | ((long) data.get(6) & 0xff) << 32 |
                        ((long) data.get(5) & 0xff) << 24
                        | ((long) data.get(4) & 0xff) << 16 |
                        ((long) data.get(3) & 0xff) << 8 |
                        ((long) data.get(2) & 0xff);
    }

    public Detection toDetection() {
        return new Detection((int) batonTag, (int) beaconTag,
                new Timestamp(timestamp));
    }

    @Override
    public String toString() {
        return String.format("Beacon %o: runner: %o at %d", this.beaconTag,
                this.batonTag, this.timestamp);
    }
}
