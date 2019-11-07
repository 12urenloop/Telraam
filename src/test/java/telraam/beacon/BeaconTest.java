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
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.BeforeAll;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
* Beacon integration test.
* Spoofing ServerSocket and Socket so you can write to it at will.
* TODO: Test socket exception, but I don't really know what could fail.
*
* @author  Arthur Vercruysse
*/
public class BeaconTest {
    private static Logger logger = Logger.getLogger(BeaconTest.class.getName());

    private static final Semaphore barrier = new Semaphore(8);

    static List<OurSocket> connectedSockets = new ArrayList<>();

    public static class OurSocket extends Socket {
        private PipedInputStream pis;
        private PipedOutputStream pos;

        public OurSocket() throws IOException {
            super();
            barrier.acquireUninterruptibly();

            pis = new PipedInputStream();
            pos = new PipedOutputStream(pis);
        }

        public InputStream getInputStream() throws IOException {
            return this.pis;
        }

        public void write(byte[] buf, boolean acq) throws IOException {
            if (acq) {
                barrier.acquireUninterruptibly();
            }

            pos.write(buf);
            pos.flush();
        }

        public void close() throws IOException {
            barrier.acquireUninterruptibly();
            pos.close();
            pis.close();
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
                barrier.release();
                while (true) {
                    try {
                        Thread.sleep(2000);
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
    static AtomicInteger data = new AtomicInteger();
    static AtomicInteger connects = new AtomicInteger();
    static AtomicInteger errors = new AtomicInteger();
    static AtomicInteger exits = new AtomicInteger();

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
            connects.incrementAndGet();
            barrier.release();
            return null;
        });

        ba.onData((_e) -> {
            data.incrementAndGet();
            barrier.release();
            return null;
        });

        ba.onDisconnect((_e) -> {
            exits.incrementAndGet();
            barrier.release();
            return null;
        });

        ba.onError((e) -> {
            logger.log(Level.SEVERE, "error", e);
            errors.incrementAndGet();
            return null;
        });

        barrier.acquire();
        new Thread(ba).start();

        barrier.acquire(8);
        barrier.release(8);

        // Check if all beacons are connected
        assertEquals(5, connects.get());
        assertEquals(errors.get(), 0);

        // Check if they can disconnect at will
        connectedSockets.remove(0).close();

        barrier.acquire(8);
        barrier.release(8);

        assertEquals(exits.get(), 1);
        assertEquals(errors.get(), 0);

        // Check if no beacon messages are sent with incomplete data
        // Aka do they buffer correctly?
        for (OurSocket s: connectedSockets) {
            s.write("hadeksfd".getBytes(), false);
        }

        barrier.acquire(8);
        barrier.release(8);

        assertEquals(data.get(), 0);
        assertEquals(errors.get(), 0);

        // But not too much either
        for (OurSocket s: connectedSockets) {
            s.write("dsa".getBytes(), true);
        }

        barrier.acquire(8);
        barrier.release(8);

        assertEquals(data.get(), connectedSockets.size());
        assertEquals(errors.get(), 0);

        // Do they all close correctly
        for (OurSocket s: connectedSockets) {
            s.close();
        }

        barrier.acquire(8);
        barrier.release(8);

        assertEquals(exits.get(), 5);

        // No errors received
        assertEquals(errors.get(), 0);
    }
}
