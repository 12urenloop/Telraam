package telraam.logic;

import org.jdbi.v3.core.Jdbi;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import telraam.database.daos.LapDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.Detection;
import telraam.database.models.Lap;
import telraam.database.models.Team;
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
    }

    @Test
    void testOneBeaconShouldGenerate2Laps() {
        int beaconId1 = 1;
        int batonId1 = 1;
        int baseTime = 10000;
        // amount of milliseconds a lap should take
        int lapTime = 50000;

        // mock the teamdao so it returns at least one team
        when(mockJdbi.getMockTeamDAO().getAll())
                .thenReturn(List.of(new Team("team1", 1)));


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


}