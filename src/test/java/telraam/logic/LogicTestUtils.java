package telraam.logic;

import telraam.database.models.Beacon;
import telraam.database.models.Detection;
import telraam.mocks.MockJDBI;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.when;

public class LogicTestUtils {

    public static List<Detection> generateNiceSequence(List<Beacon> beacons,
                                                       int batonId,
                                                       int interval) {
        return LogicTestUtils
                .generateNiceSequence(beacons, batonId, interval, 1, true);
    }


    public static List<Detection> generateNiceSequence(List<Beacon> beacons,
                                                       int batonId,
                                                       int interval,
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

    public static List<Beacon> setupBeacons(MockJDBI mockJDBI, int n) {
        List<Beacon> beacons = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            Beacon beacon = new Beacon("b" + i, i * 100);
            beacon.setId(i);
            beacons.add(beacon);

        }

        when(mockJDBI.getMockBeaconDAO().getAll()).thenReturn(beacons);
        return beacons;
    }
}
