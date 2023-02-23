package telraam.logic;

import org.jdbi.v3.core.Jdbi;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.database.daos.LapDAO;
import telraam.database.daos.LapSourceDAO;
import telraam.database.models.Detection;
import telraam.database.models.Lap;
import telraam.database.models.LapSource;

import java.sql.Timestamp;
import java.util.Optional;

import static org.mockito.Mockito.*;

class SimpleLapperTest {
    private Jdbi mockJdbi;
    private LapDAO mockDAO;
    private LapSourceDAO mockSourceDAO;

    @BeforeEach
    void setUp() {
        mockJdbi = mock(Jdbi.class);
        mockDAO = mock(LapDAO.class);
        mockSourceDAO = mock(LapSourceDAO.class);
        when(mockJdbi.onDemand(LapDAO.class)).thenReturn(mockDAO);
        when(mockSourceDAO.getByName(SimpleLapper.SOURCE_NAME))
                .thenReturn(Optional.of(new LapSource(SimpleLapper.SOURCE_NAME)));
        when(mockJdbi.onDemand(LapSourceDAO.class)).thenReturn(mockSourceDAO);
    }

    @Test
    void testOneStationShouldGenerate2Laps() {
        int stationId1 = 1;
        int batonId1 = 1;
        int baseTime = 10000;
        // amount of milliseconds a lap should take
        int lapTime = 50000;


        // baton passes station 1 for the first time
        Detection d1 =
                new Detection(batonId1, stationId1, -80, 100.0f, 1L, 1, new Timestamp(baseTime));
        // baton passes station 1 for the second time
        Detection d2 = new Detection(batonId1, stationId1, -80, 100.0f, 2L, 2, new Timestamp(baseTime + lapTime));

        // baton passes station 1 for the third time
        Detection d3 = new Detection(batonId1, stationId1, -80, 100.0f, 3L, 3, new Timestamp(baseTime + lapTime + lapTime));

        Lapper lapper = new SimpleLapper(mockJdbi);

        lapper.handle(d1);
        lapper.handle(d2);
        lapper.handle(d3);

        verify(mockDAO, times(2)).insert(any(Lap.class));

    }


}
