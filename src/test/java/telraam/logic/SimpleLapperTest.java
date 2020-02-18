package telraam.logic;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.database.models.Beacon;
import telraam.database.models.Detection;
import telraam.database.models.Lap;
import telraam.database.models.Team;
import telraam.mocks.MockJDBI;

import java.sql.Timestamp;
import java.util.List;

import static org.mockito.Mockito.*;
import static telraam.logic.LogicTestUtils.generateNiceSequence;
import static telraam.logic.LogicTestUtils.setupBeacons;

class SimpleLapperTest {
    private MockJDBI mockJdbi;

    @BeforeEach
    void setUp() {
        mockJdbi = new MockJDBI();

        // mock the teamdao so it returns at least one team
        when(mockJdbi.getMockTeamDAO().getAll())
                .thenReturn(List.of(new Team("team1", 1)));
    }

    @Test
    void testOneBeaconShouldGenerate2Laps() {
        int batonId1 = 1;
        int baseTime = 10000;
        // amount of milliseconds a lap should take
        int lapTime = 50000;
        List<Beacon> beacons = setupBeacons(mockJdbi, 1);
        int beaconId1 = beacons.get(0).getId();


        // baton passes station 1 for the first time
        Detection d1 =
                new Detection(batonId1, beaconId1, new Timestamp(baseTime));
        // baton passes station 1 for the second time
        Detection d2 = new Detection(batonId1, beaconId1,
                new Timestamp(baseTime + lapTime));

        // baton passes station 1 for the third time
        Detection d3 = new Detection(batonId1, beaconId1,
                new Timestamp(baseTime + lapTime + lapTime));

        Lapper lapper = new SimpleLapper(mockJdbi.getMockJdbi());

        lapper.handle(d1);
        lapper.handle(d2);
        lapper.handle(d3);

        verify(mockJdbi.getMockLapDAO(), times(2)).insert(any(Lap.class));
    }

    @Test
    void testShouldNotGenerateALapHasntPassedFinish() {
        List<Beacon> beacons = setupBeacons(mockJdbi, 2);
        int baton1 = 1;
        int baseTime = 10000;
        int lapTime = 50000;
        int b1 = beacons.get(0).getId();
        int b2 = beacons.get(1).getId();

        Detection d1 = new Detection(baton1, b1, new Timestamp(baseTime));
        Detection d2 = new Detection(baton1, b2,
                new Timestamp(baseTime + (lapTime / 2)));
        Lapper lapper = new SimpleLapper(mockJdbi.getMockJdbi());
        lapper.handle(d1);
        lapper.handle(d2);
        verify(mockJdbi.getMockLapDAO(), never()).insert(any(Lap.class));

    }

    @Test
    void testShouldNotLapWhenDuplicateDetection() {
        int beacon1 = 1;
        int baton1 = 1;
        int baseTime = 10000;
        setupBeacons(mockJdbi, 3);

        Detection d1 = new Detection(baton1, beacon1, new Timestamp(baseTime));
        Detection d2 = new Detection(baton1, beacon1,
                new Timestamp(baseTime));
        Lapper lapper = new SimpleLapper(mockJdbi.getMockJdbi());
        lapper.handle(d1);
        lapper.handle(d2);
        verify(mockJdbi.getMockLapDAO(), never()).insert(any(Lap.class));

    }

    @Test
    void testShouldGenerateALapWith5Beacons() {
        List<Beacon> beacons = setupBeacons(mockJdbi, 5);
        List<Detection> detections = generateNiceSequence(beacons, 1, 1000);
        Lapper lapper = new SimpleLapper(mockJdbi.getMockJdbi());
        for (Detection detection : detections) {
            lapper.handle(detection);
        }

        verify(mockJdbi.getMockLapDAO(), times(1)).insert(any(Lap.class));

    }


}