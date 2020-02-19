package telraam.logic;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.dropwizard.jackson.Jackson;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.database.models.*;
import telraam.mocks.MockJDBI;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;
import static telraam.logic.LogicTestUtils.generateNiceSequence;
import static telraam.logic.LogicTestUtils.setupBeacons;

public class DistanceLapperTest {
    private MockJDBI mockJDBI;

    @BeforeEach
    void setUp() {
        mockJDBI = new MockJDBI();

        // mock the teamdao so it returns at least one team
        when(mockJDBI.getMockTeamDAO().getAll())
                .thenReturn(List.of(new Team("team1", 1)));

        // make sure baton 1 and team 1 are available
        Baton b1 = new Baton("baton1");
        b1.setId(1);

        when(mockJDBI.getMockBatonDAO().getAll())
                .thenReturn(List.of(b1));
        when(mockJDBI.getMockTeamDAO().getByBatonId(1))
                .thenReturn(Optional.of(new Team("test", 1)));
    }

    @Test
    void testShouldGenerateALapWith5Beacons() {
        List<Beacon> beacons = setupBeacons(mockJDBI, 5);
        List<Detection> detections = generateNiceSequence(beacons, 1, 1000);
        Lapper lapper = new DistanceLapper(mockJDBI.getMockJdbi());
        for (Detection detection : detections) {
            lapper.handle(detection);
        }

        verify(mockJDBI.getMockLapDAO(), times(1)).insert(any(Lap.class));

    }

    @Test
    void testShouldGenerateLapWhenBeaconsMissing() {
        List<Beacon> beacons = setupBeacons(mockJDBI, 5);
        List<Detection> detections = List.of(
                new Detection(1, 1, 1, new Timestamp(1000)),
                new Detection(2, 1, 2, new Timestamp(2000)),
                new Detection(4, 1, 1, new Timestamp(4000))
        );
        Lapper lapper = new DistanceLapper(mockJDBI.getMockJdbi());
        for (Detection detection : detections) {
            lapper.handle(detection);
        }

        verify(mockJDBI.getMockLapDAO(), times(1)).insert(any(Lap.class));

    }

    @Test
    void testShouldGenerateLapWithOnly2Detections() {
        List<Beacon> beacons = setupBeacons(mockJDBI, 5);
        List<Detection> detections = List.of(
                new Detection(1, 1, 1, new Timestamp(1000)),
                new Detection(4, 1, 1, new Timestamp(4000))
        );
        Lapper lapper = new DistanceLapper(mockJDBI.getMockJdbi());
        for (Detection detection : detections) {
            lapper.handle(detection);
        }

        verify(mockJDBI.getMockLapDAO(), times(1)).insert(any(Lap.class));

    }

    @Test
    void testShouldNotGenerateLapWhenSpeedOfLight() {
        List<Beacon> beacons = setupBeacons(mockJDBI, 5);
        List<Detection> detections = List.of(
                new Detection(1, 1, 1, new Timestamp(1000)),
                new Detection(4, 1, 1, new Timestamp(1001))
        );
        Lapper lapper = new DistanceLapper(mockJDBI.getMockJdbi());
        for (Detection detection : detections) {
            lapper.handle(detection);
        }

        verify(mockJDBI.getMockLapDAO(), never()).insert(any(Lap.class));

    }

    @Test
    void testShouldNotGenerateLapWhenGoingBackInTime() {
        List<Beacon> beacons = setupBeacons(mockJDBI, 5);
        List<Detection> detections = List.of(
                new Detection(1, 1, 1, new Timestamp(1000)),
                new Detection(4, 1, 1, new Timestamp(999))
        );
        Lapper lapper = new DistanceLapper(mockJDBI.getMockJdbi());
        for (Detection detection : detections) {
            lapper.handle(detection);
        }

        verify(mockJDBI.getMockLapDAO(), never()).insert(any(Lap.class));

    }

    @Test
    void testShouldNotGenerateLapWhenNotPastFinish() {
        List<Beacon> beacons = setupBeacons(mockJDBI, 5);
        // there are 5 beacons but only the first four send a message
        List<Detection> detections = List.of(
                new Detection(1, 1, 1, new Timestamp(1000)),
                new Detection(2, 1, 2, new Timestamp(2000)),
                new Detection(3, 1, 3, new Timestamp(3000)),
                new Detection(4, 1, 4, new Timestamp(4000))
        );
        Lapper lapper = new DistanceLapper(mockJDBI.getMockJdbi());

        for (Detection detection : detections) {
            lapper.handle(detection);
        }

        verify(mockJDBI.getMockLapDAO(), never()).insert(any(Lap.class));
    }

    @Test
    void testShouldNotGenerateLapWhenDuplicates() {
        List<Beacon> beacons = setupBeacons(mockJDBI, 5);
        // there are 5 beacons but only the first four send a message
        // some beacons send multiple messages in short succession
        List<Detection> detections = List.of(
                new Detection(1, 1, 1, new Timestamp(1000)),
                new Detection(2, 1, 2, new Timestamp(2000)),
                new Detection(2, 1, 2, new Timestamp(2100)),
                new Detection(2, 1, 2, new Timestamp(2200)),
                new Detection(2, 1, 2, new Timestamp(2300)),
                new Detection(3, 1, 3, new Timestamp(3000)),
                new Detection(3, 1, 3, new Timestamp(3100)),
                new Detection(3, 1, 3, new Timestamp(3200)),
                new Detection(3, 1, 3, new Timestamp(3300)),
                new Detection(4, 1, 4, new Timestamp(4000))
        );
        Lapper lapper = new DistanceLapper(mockJDBI.getMockJdbi());

        for (Detection detection : detections) {
            lapper.handle(detection);
        }

        verify(mockJDBI.getMockLapDAO(), never()).insert(any(Lap.class));
    }
}
