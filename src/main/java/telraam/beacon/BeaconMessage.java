package telraam.beacon;

/**
* BeaconMessage is the representation of what is received from a beacon.
* This should parse the incoming byte[].
*
* @author  Arthur Vercruysse
*/
public class BeaconMessage {
    public static final int MESSAGESIZE = 10;

    public byte[] data;

    // DO NOT STORE THIS DATA, IT WILL BE OVERWRITTEN
    public BeaconMessage(byte[] data) {
        this.data = data;
    }
}
