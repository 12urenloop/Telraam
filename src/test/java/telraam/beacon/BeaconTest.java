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
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.BeforeAll;

/**
 * Beacon integration test. Spoofing ServerSocket and Socket so you can write to
 * it at will. TODO: Test socket exception, but I don't really know what could
 * fail.
 *
 * @author Arthur Vercruysse
 */
class BeaconTest {
    private static final Semaphore barrier = new Semaphore(8);

    static List<OurSocket> connectedSockets = new ArrayList<>();
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
    void testEverythingBeacon() throws Exception {

        ba.onConnect((_e) -> {
            connects.incrementAndGet();
            barrier.release();
            return null;
        });

        ba.onData((event) -> {
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
            errors.incrementAndGet();
            barrier.release();
            return null;
        });

        barrier.acquire();
        new Thread(ba).start();

        barrier.acquire(8);
        barrier.release(8);

        // Check if all beacons are connected
        assertEquals(5, connects.get());
        assertEquals(0, errors.get());

        // Check if they can disconnect at will
        connectedSockets.remove(0).close();

        barrier.acquire(8);
        barrier.release(8);

        assertEquals(1, exits.get());
        assertEquals(0, errors.get());

        // Check if no beacon messages are sent with incomplete data
        // Aka do they buffer correctly?
        for (OurSocket s : connectedSockets) {
            ByteBuffer buf = ByteBuffer.allocate(12);
            buf.putShort(0, (byte) 0);
            buf.putShort(2, (byte) 5);
            buf.putLong(4, new Date().getTime());

            s.write("<<<<".getBytes(), false);
            s.write(buf.array(), false);
        }

        barrier.acquire(8);
        barrier.release(8);

        assertEquals(0, data.get());
        assertEquals(0, errors.get());

        // But not too much either
        for (OurSocket s : connectedSockets) {
            s.write(">>>>".getBytes(), true);
        }

        barrier.acquire(8);
        barrier.release(8);

        assertEquals(data.get(), connectedSockets.size());
        assertEquals(0, errors.get());

        // Test invalid msg send

        // Invalid message
        connectedSockets.get(0).write("<<<<fsdtestds>>>>".getBytes(), true);

        barrier.acquire(8);
        barrier.release(8);
        assertEquals(1, errors.get());

        // No opening tag
        connectedSockets.get(0).write("<<<jkfd;ajk;bjkea>>>>".getBytes(), true);

        barrier.acquire(8);
        barrier.release(8);
        assertEquals(2, errors.get());

        // 2 Opening tags
        connectedSockets.get(0).write("<<<<jkdfasbjkea<<<<fdsdtestds".getBytes(), true);

        barrier.acquire(8);
        barrier.release(8);
        assertEquals(3, errors.get());

        // Do they all close correctly
        for (OurSocket s : connectedSockets) {
            s.close();
        }

        barrier.acquire(8);
        barrier.release(8);

        assertEquals(5, exits.get());

        // No errors received
        assertEquals(3, errors.get());
    }

    @Test
    void testBeaconFormat() throws Exception {
        ByteBuffer buf = ByteBuffer.allocate(BeaconMessage.MESSAGESIZE);
        buf.putShort(0, (short) 10);
        buf.putShort(2, (short) 13);
        buf.putLong(4, 1583267378714L);

        BeaconMessage msg = new BeaconMessage(buf);

        assertEquals(10, msg.getBeaconTag());
        assertEquals(13, msg.getBatonTag());
        assertEquals(1583267378714L, msg.getTimestamp());
    }

    @Test
    void testBeaconMessageSize() throws Exception {
        ByteBuffer buf = ByteBuffer.allocate(BeaconMessage.MESSAGESIZE - 2);
        assertThrows(BeaconException.class, () -> {
            new BeaconMessage(buf);
        });
    }

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
}
