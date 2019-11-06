package telraam.beacon;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.io.InputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.lang.reflect.Field;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeAll;

/**
* Beacon integration test.
* Spoofing ServerSocket and Socket so you can write to it at will.
* TODO: Test socket exception, but I don't really know what could fail.
*
* @author  Arthur Vercruysse
*/
public class BeaconTest {

    static List<OurSocket> connectedSockets = new ArrayList<>();

    public static class OurSocket extends Socket {
        private PipedInputStream pis;
        private PipedOutputStream pos;

        public OurSocket() throws IOException {
            super();

            pis = new PipedInputStream();
            pos = new PipedOutputStream(pis);
        }

        public InputStream getInputStream() throws IOException {
            return this.pis;
        }

        public void write(byte[] buf) throws IOException {
            pos.write(buf);
            pos.flush();
        }

        public void close() throws IOException {
            pos.close();
            pis.close();
            super.close();
        }
    }

    public static class OurServerSocket extends ServerSocket {
        private int connections;

        public OurServerSocket(int connections) throws IOException {
            super();
            this.connections = connections;
        }

        @Override
        public Socket accept() throws IOException {
            // Only spawn connections amount of sockets
            if (connections < 1) {
                while (true) {
                    try {
                        Thread.sleep(1000);
                    } catch (Exception e) {
                    }
                }
            }
            connections--;
            OurSocket s = new OurSocket();
            // super.implAccept(s); // This fails, and should not be called
            connectedSockets.add(s);
            return s;
        }
    }

    static BeaconAggregator ba;
    static int data, connects, errors, exits;

    @BeforeAll
    public static void init() throws Exception {
        ba = new BeaconAggregator(-1);

        Field socketField = ba.getClass().getSuperclass().getDeclaredField("socket");

        socketField.setAccessible(true);
        socketField.set(ba, new OurServerSocket(5));
    }

    @Test
    public void testEverythingBeacon() throws Exception {

        ba.onConnect((_e) -> {
            connects += 1;
            return null;
        });

        ba.onData((_e) -> {
            data += 1;
            return null;
        });

        ba.onDisconnect((_e) -> {
            exits += 1;
            return null;
        });

        ba.onError((_e) -> {
            errors += 1;
            return null;
        });

        new Thread(ba).start();
        Thread.sleep(100);

        // Check if all beacons are connected
        assertEquals(connects, 5);

        // Check if they can disconnect at will
        connectedSockets.remove(0).close();
        Thread.sleep(100);
        assertEquals(exits, 1);

        // Check if no beacon messages are sent with incomplete data
        // Aka do they buffer correctly?
        for (OurSocket s: connectedSockets) {
            s.write("hadeksfd".getBytes());
        }
        Thread.sleep(100);
        assertEquals(data, 0);

        // But not too much either
        for (OurSocket s: connectedSockets) {
            s.write("dsa".getBytes());
        }

        Thread.sleep(100);
        assertEquals(data, connectedSockets.size());

        // Do they all close correctly
        for (OurSocket s: connectedSockets) {
            s.close();
        }
        Thread.sleep(100);
        assertEquals(exits, 5);

        // No errors received
        assertEquals(errors, 0);
    }
}
