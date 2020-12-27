package telraam.beacon;

import telraam.database.models.Detection;

import java.nio.ByteBuffer;
import java.sql.Timestamp;

/**
 * BeaconMessage is the representation of what is received from a beacon. This
 * should parse the incoming byte.get().
 *
 * @author Arthur Vercruysse
 */
public class BeaconMessage {
    public static final int MESSAGESIZE = Short.BYTES + Short.BYTES + Long.BYTES;

    public static final byte[] STARTTAG = {'<', '<', '<', '<'};
    public static final byte[] ENDTAG = {'>', '>', '>', '>'};

    public short beaconTag;
    public short batonTag;
    public long timestamp;

    public BeaconMessage(ByteBuffer buffer) throws BeaconException {
        if (buffer.capacity() < MESSAGESIZE) {
            throw new BeaconException.MsgToShort(MESSAGESIZE, buffer.capacity());
        }

        beaconTag = buffer.getShort(Short.BYTES * 0);
        batonTag = buffer.getShort(Short.BYTES * 1);
        timestamp = buffer.getLong(Short.BYTES * 2);
    }

    public Detection toDetection() {
        return new Detection((int) batonTag, (int) beaconTag,
                new Timestamp(timestamp));
    }

    @Override
    public String toString() {
        return String.format("Beacon %o: runner: %o at %s", this.beaconTag,
                this.batonTag, new Timestamp(this.timestamp));
    }
}
