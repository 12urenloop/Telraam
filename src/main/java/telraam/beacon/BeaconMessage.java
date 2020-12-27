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

    protected static final byte[] STARTTAG = {'<', '<', '<', '<'};
    protected static final byte[] ENDTAG = {'>', '>', '>', '>'};

    private short beaconTag;
    private short batonTag;
    private long timestamp;

    public BeaconMessage(ByteBuffer buffer) throws BeaconException {
        if (buffer.capacity() < MESSAGESIZE) {
            throw new BeaconException.MsgToShort(MESSAGESIZE, buffer.capacity());
        }

        setBeaconTag(buffer.getShort(0));
        setBatonTag(buffer.getShort(Short.BYTES));
        setTimestamp(buffer.getLong(Short.BYTES * 2));
    }

    public Detection toDetection() {
        return new Detection((int) getBatonTag(), (int) getBeaconTag(),
                new Timestamp(getTimestamp()));
    }

    @Override
    public String toString() {
        return String.format("Beacon %o: runner: %o at %s", this.getBeaconTag(),
                this.getBatonTag(), new Timestamp(this.getTimestamp()));
    }

    public short getBeaconTag() {
        return beaconTag;
    }

    public void setBeaconTag(short beaconTag) {
        this.beaconTag = beaconTag;
    }

    public short getBatonTag() {
        return batonTag;
    }

    public void setBatonTag(short batonTag) {
        this.batonTag = batonTag;
    }

    public long getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(long timestamp) {
        this.timestamp = timestamp;
    }
}
