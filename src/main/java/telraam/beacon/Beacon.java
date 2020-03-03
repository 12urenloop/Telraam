package telraam.beacon;

import java.io.IOException;
import java.io.InputStream;
import java.io.EOFException;
import java.net.Socket;
import java.util.List;
import java.util.ArrayList;

/**
 * Beacon is socket wrapper that listens to the sockets and emits BeaconMessages
 * when enough bytes are read.
 *
 * Beacons are closed at the first Exception encountered. This could be changed
 * if need be.
 *
 * @author Arthur Vercruysse
 */
public class Beacon extends EventGenerator<BeaconMessage> implements Runnable {
    private static final int messageSize = BeaconMessage.MESSAGESIZE;
    private static final byte[] startTag = BeaconMessage.STARTTAG;
    private static final byte[] endTag = BeaconMessage.ENDTAG;

    private InputStream inputStream;
    private boolean isReadingMessage;
    private List<Byte> msgBuf;
    private int startTagIndex, endTagIndex;

    public Beacon(Socket socket, Callback<Void, Event<BeaconMessage>> h) throws IOException {
        super(h);

        this.inputStream = socket.getInputStream();
        this.isReadingMessage = false;
        this.msgBuf = new ArrayList<>(messageSize);
        this.startTagIndex = 0;
        this.endTagIndex = 0;

        // TODO: wait what, a thread per beacon? How about async handling?
        new Thread(this).start();
    }

    // Handle possible advancement in the start tag
    private void handleStartTag(byte b) {
        if (b == startTag[startTagIndex]) {
            startTagIndex++;

            // A complete start tag is found
            // Delete current msgBuf content and start over
            if (startTagIndex == startTag.length) {
                startTagIndex = 0;

                if (this.isReadingMessage) {
                    // TODO: Maybe we want to reset msgBuf idk
                    this.error(new BeaconException.MsgStartWithNoEnd());
                } else {
                    msgBuf.clear();
                    this.isReadingMessage = true;
                }
            }
        } else {
            startTagIndex = 0;
        }
    }

    // Handle possible advancement in the end tag
    private void handleEndTag(byte b) {
        if (b == endTag[endTagIndex]) {
            endTagIndex++;

            // A complete end tag is found
            // Flush the msgBuffer
            if (endTagIndex == endTag.length) {
                endTagIndex = 0;

                if (isReadingMessage) {

                    // Remove end tag from message
                    for (int k = 0; k < endTag.length; k++) {
                        msgBuf.remove(msgBuf.size() - 1);
                    }

                    // Catch errors thrown at message decoding and propagate
                    try {
                        this.data(new BeaconMessage(msgBuf));
                    } catch (Exception e) {
                        this.error(e);
                    }

                    isReadingMessage = false;
                } else {
                    this.error(new BeaconException.MsgEndWithNoStart());
                }
            }
        } else {
            endTagIndex = 0;
        }
    }

    public void run() {
        this.connect();

        byte[] buf = new byte[1024];

        try {
            while (true) {
                int c = this.inputStream.read(buf);
                if (c < 0)
                    throw new EOFException();

                for (int i = 0; i < c; i++) {
                    byte b = buf[i];
                    this.msgBuf.add(b);

                    this.handleStartTag(b);

                    this.handleEndTag(b);
                }
            }
        } catch (IOException e) {
            exit();
        }
    }
}
