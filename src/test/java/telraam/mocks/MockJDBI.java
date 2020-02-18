package telraam.mocks;

import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.*;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class MockJDBI {

    private Jdbi mockJdbi;
    private BatonDAO mockBatonDAO;
    private BeaconDAO mockBeaconDAO;
    private DetectionDAO mockDetectionDAO;
    private LapDAO mockLapDAO;
    private TeamDAO mockTeamDAO;

    public MockJDBI() {
        mockJdbi = mock(Jdbi.class);
        mockBatonDAO = mock(BatonDAO.class);
        mockBeaconDAO = mock(BeaconDAO.class);
        mockDetectionDAO = mock(DetectionDAO.class);
        mockLapDAO = mock(LapDAO.class);
        mockTeamDAO = mock(TeamDAO.class);

    }

    public Jdbi getMockJdbi() {

        when(mockJdbi.onDemand(BatonDAO.class)).thenReturn(mockBatonDAO);
        when(mockJdbi.onDemand(BeaconDAO.class)).thenReturn(mockBeaconDAO);
        when(mockJdbi.onDemand(DetectionDAO.class))
                .thenReturn(mockDetectionDAO);
        when(mockJdbi.onDemand(LapDAO.class)).thenReturn(mockLapDAO);
        when(mockJdbi.onDemand(TeamDAO.class)).thenReturn(mockTeamDAO);
        return mockJdbi;

    }

    public void setMockJdbi(Jdbi mockJdbi) {
        this.mockJdbi = mockJdbi;
    }

    public BatonDAO getMockBatonDAO() {
        return mockBatonDAO;
    }

    public void setMockBatonDAO(BatonDAO mockBatonDAO) {
        this.mockBatonDAO = mockBatonDAO;
    }

    public BeaconDAO getMockBeaconDAO() {
        return mockBeaconDAO;
    }

    public void setMockBeaconDAO(BeaconDAO mockBeaconDAO) {
        this.mockBeaconDAO = mockBeaconDAO;
    }

    public DetectionDAO getMockDetectionDAO() {
        return mockDetectionDAO;
    }

    public void setMockDetectionDAO(DetectionDAO mockDetectionDAO) {
        this.mockDetectionDAO = mockDetectionDAO;
    }

    public LapDAO getMockLapDAO() {
        return mockLapDAO;
    }

    public void setMockLapDAO(LapDAO mockLapDAO) {
        this.mockLapDAO = mockLapDAO;
    }

    public TeamDAO getMockTeamDAO() {
        return mockTeamDAO;
    }

    public void setMockTeamDAO(TeamDAO mockTeamDAO) {
        this.mockTeamDAO = mockTeamDAO;
    }
}
