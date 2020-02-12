package telraam.logic;

import org.jdbi.v3.core.Jdbi;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import telraam.database.daos.LapDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.*;
import telraam.mocks.MockJDBI;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.*;

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
        List<Beacon> beacons = setupBeacons(1);
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
        List<Beacon> beacons = setupBeacons(2);
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
        setupBeacons(3);

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
        List<Beacon> beacons = setupBeacons(5);
        List<Detection> detections = generateNiceSequence(beacons, 1, 1000);
        Lapper lapper = new SimpleLapper(mockJdbi.getMockJdbi());
        for (Detection detection : detections) {
            lapper.handle(detection);
        }

        verify(mockJdbi.getMockLapDAO(), times(1)).insert(any(Lap.class));

    }

    private List<Detection> generateNiceSequence(List<Beacon> beacons,
                                                 int batonId, int interval) {
        return generateNiceSequence(beacons, batonId, interval, 1, true);
    }


    private List<Detection> generateNiceSequence(List<Beacon> beacons,
                                                 int batonId, int interval,
                                                 int times,
                                                 boolean completeLap) {
        List<Detection> rslt = new ArrayList<>();
        for (int t = 0; t < times; t++) {
            for (int i = 0; i < beacons.size(); i++) {
                rslt.add(new Detection(batonId, beacons.get(i).getId(),
                        new Timestamp((i + 1) * interval)));

            }
        }
        if (completeLap) {
            rslt.add(new Detection(batonId, beacons.get(0).getId(),
                    new Timestamp(
                            rslt.get(rslt.size() - 1).getTimestamp().getTime() +
                                    interval)));
        }
        return rslt;

    }

    private List<Beacon> setupBeacons(int n) {
        List<Beacon> beacons = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            Beacon beacon = new Beacon("b" + i);
            beacon.setId(i);
            beacons.add(beacon);

        }

        when(mockJdbi.getMockBeaconDAO().getAll()).thenReturn(beacons);
        return beacons;
    }


}