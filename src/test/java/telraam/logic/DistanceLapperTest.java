package telraam.logic;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.database.models.*;
import telraam.mocks.MockJDBI;

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
    }

    @Test
    void testShouldGenerateALapWith5Beacons() {
        List<Beacon> beacons = setupBeacons(mockJDBI, 5);
        Baton b1 = new Baton("baton1");
        b1.setId(1);
        when(mockJDBI.getMockBatonDAO().getAll())
                .thenReturn(List.of(b1));
        when(mockJDBI.getMockTeamDAO().getByBatonId(1))
                .thenReturn(Optional.of(new Team("test", 1)));
        List<Detection> detections = generateNiceSequence(beacons, 1, 1000);
        Lapper lapper = new DistanceLapper(mockJDBI.getMockJdbi());
        for (Detection detection : detections) {
            lapper.handle(detection);
        }

        verify(mockJDBI.getMockLapDAO(), times(1)).insert(any(Lap.class));

    }
}
