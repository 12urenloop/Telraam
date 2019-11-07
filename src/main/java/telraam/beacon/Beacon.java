package telraam.beacon;

import java.io.IOException;
import java.io.InputStream;
import java.io.EOFException;
import java.net.Socket;

/**
* Beacon is socket wrapper that listens to the sockets
* and emits BeaconMessages when enough bytes are read.
*
* Beacons are closed at the first Exception encountered.
* This could be changed if need be.
*
* @author  Arthur Vercruysse
*/
public class Beacon extends EventGenerator<BeaconMessage> implements Runnable {
    private Socket s;
    private int messageSize = BeaconMessage.MESSAGESIZE;

    public Beacon(Socket socket, Callback<Void, Event<BeaconMessage>> h) {
        super(h);

        this.s = socket;

        new Thread(this).start();
    }

    public void run() {
        this.connect();

        byte[] buf = new byte[messageSize];
        int at = 0;
        InputStream is;

        try {
            is = s.getInputStream();
        } catch (IOException e) {
            error(e);
            return;
        }

        try {
            while (true) {
                int c = is.read(buf, at, messageSize - at);
                if (c < 0) throw new EOFException();
                at += c;
                if (at == messageSize) {
                    this.data(new BeaconMessage(buf));
                    at = 0;
                }
            }
        } catch (IOException e) {
            exit();
        }
    }
}
