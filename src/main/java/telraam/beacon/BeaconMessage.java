package telraam.beacon;

import telraam.database.models.Detection;

import java.nio.ByteBuffer;
import java.sql.Timestamp;
import java.text.ParseException;
import java.util.Date;

/**
 * BeaconMessage is the representation of what is received from a beacon. This
 * should parse the incoming byte.get().
 *
 * @author Arthur Vercruysse
 * 
 * We currently parse for the old CVC beacon messages.
 * STATION_MAC,IGNORE,BATTON_MAC,RSSI\n
 */
public class BeaconMessage {
    public static final int MESSAGESIZE = 0;

    public static final byte[] STARTTAG = {};
    public static final byte[] ENDTAG = {'\n'};

    public String stationMAC;
    public String battonMAC;
    public String RSSI;
    public Timestamp time;

    public BeaconMessage(ByteBuffer buffer) throws BeaconException {
        // if (buffer.capacity() < MESSAGESIZE) {
        //     throw new BeaconException.MsgToShort(MESSAGESIZE, buffer.capacity());
        // }

        String sBuffer = new String(buffer.array());
        System.out.println("Got msg "+sBuffer);
        String[] parts = sBuffer.split(",");
        if (parts.length != 4) {
            throw new BeaconException.ParseError(sBuffer, "Not enough parts");
        }

        if (parts[1].equalsIgnoreCase("ignore")) {
            time = new Timestamp(new Date().getTime());
        } else {
            try {
                time = new Timestamp(Long.parseLong(parts[1]));
            } catch (NumberFormatException e) {
                throw new BeaconException.ParseError(parts[1], "Not \"ignore\" and could not be parsed");
            }
        }

        stationMAC = parts[0];
        battonMAC = parts[2];
        RSSI = parts[3];
    }

    // public Detection toDetection() {
    //     return new Detection(battonMAC, stationMAC, RSSI, time);
    // }

    @Override
    public String toString() {
        return String.format("Beacon %s: runner: %s with rssi %s", this.stationMAC,
                this.battonMAC, this.RSSI);
    }
}
