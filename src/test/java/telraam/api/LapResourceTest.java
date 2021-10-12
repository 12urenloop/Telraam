package telraam.api;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.DatabaseTest;
import telraam.database.daos.LapDAO;
import telraam.database.daos.LapSourceDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.Lap;
import telraam.database.models.LapSource;
import telraam.database.models.Team;

import java.sql.Timestamp;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class LapResourceTest extends DatabaseTest {

    private final Timestamp exampleTime = new Timestamp(123456789);
    private LapSourceDAO sourceDAO;
    private LapDAO lapDAO;
    private Team testTeam;

    @Override
    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        sourceDAO = jdbi.onDemand(LapSourceDAO.class);
        lapDAO = jdbi.onDemand(LapDAO.class);
        var teamDAO = jdbi.onDemand(TeamDAO.class);
        testTeam = new Team("Test");
        testTeam.setId(teamDAO.insert(testTeam));
    }

    @Test
    public void testGetBySource() {
        var oneSource = new LapSource("one");
        oneSource.setId(sourceDAO.insert(oneSource));
        var secondSource = new LapSource("two");
        secondSource.setId(sourceDAO.insert(secondSource));

        var lap1 = new Lap(testTeam.getId(), oneSource.getId(), exampleTime);
        var lap2 = new Lap(testTeam.getId(), secondSource.getId(), exampleTime);
        lap1.setId(lapDAO.insert(lap1));
        lap2.setId(lapDAO.insert(lap2));

        var resource = new LapResource(lapDAO);
        var result = resource.getListOf(oneSource.getId());

        assertEquals(1, result.size());
        assertEquals(lap1.getId(), result.get(0).getId());
    }

    @Test
    public void testGetAll() {
        var oneSource = new LapSource("one");
        oneSource.setId(sourceDAO.insert(oneSource));
        var secondSource = new LapSource("two");
        secondSource.setId(sourceDAO.insert(secondSource));

        var lap1 = new Lap(testTeam.getId(), oneSource.getId(), exampleTime);
        var lap2 = new Lap(testTeam.getId(), secondSource.getId(), exampleTime);
        lap1.setId(lapDAO.insert(lap1));
        lap2.setId(lapDAO.insert(lap2));

        var resource = new LapResource(lapDAO);
        var result = resource.getListOf(null);

        assertEquals(2, result.size());
    }
}
