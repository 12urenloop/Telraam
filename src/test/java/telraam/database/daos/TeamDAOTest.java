package telraam.database.daos;

import org.jdbi.v3.core.statement.UnableToExecuteStatementException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.DatabaseTest;
import telraam.database.models.Baton;
import telraam.database.models.Team;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class TeamDAOTest extends DatabaseTest {

    private TeamDAO teamDAO;
    private BatonDAO batonDAO;

    @Override
    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        teamDAO = jdbi.onDemand(TeamDAO.class);
        batonDAO = jdbi.onDemand(BatonDAO.class);
    }

    @Test
    void createTeam() {
        Team testteam = new Team("testteam");
        final int testId = teamDAO.insert(testteam);
        assertTrue(testId > 0);

        Optional<Team> teamOptional = teamDAO.getById(testId);
        assertFalse(teamOptional.isEmpty());
        Team team = teamOptional.get();
        assertEquals("testteam", team.getName());
    }

    @Test
    void testCreateTeamWithBaton() {
        Baton testBaton = new Baton("testbaton");
        int batonId = batonDAO.insert(testBaton);
        Team testteam = new Team("testteam", batonId);
        int testId = teamDAO.insert(testteam);

        assertTrue(testId > 0);
        Optional<Team> teamOptional = teamDAO.getById(testId);
        assertFalse(teamOptional.isEmpty());
        Team team = teamOptional.get();
        assertEquals(batonId, team.getBatonId());
        assertEquals("testteam", team.getName());
    }

    @Test
    void testInsertFailsWhenNoName() {
        Team testteam = new Team();
        assertThrows(UnableToExecuteStatementException.class,
                () -> teamDAO.insert(testteam));

    }

    @Test
    void testInsertFailsWhenInvalidBaton() {
        Team testteam = new Team("testtteam", 1);
        assertThrows(UnableToExecuteStatementException.class,
                () -> teamDAO.insert(testteam));

    }

    @Test
    void testListTeamsEmpty() {
        List<Team> teams = teamDAO.getAll();
        assertNotNull(teams);
        assertEquals(0, teams.size());
    }

    @Test
    void testList2Teams() {
        Team b1 = new Team("b1");
        Team b2 = new Team("b2");
        teamDAO.insert(b1);
        teamDAO.insert(b2);

        List<Team> teams = teamDAO.getAll();
        assertNotNull(teams);
        assertEquals(2, teams.size());
        assertNotNull(
                teams.stream().filter(team -> team.getName().equals("b1")));
        assertNotNull(
                teams.stream().filter(team -> team.getName().equals("b2")));
    }

    @Test
    void testFindByIdNullWhenNoTeam() {
        Optional<Team> teamOptional = teamDAO.getById(1);
        assertTrue(teamOptional.isEmpty());
    }

    @Test
    void testUpdateDoesUpdate() {
        Team testTeam = new Team("preupdate");
        int testid = teamDAO.insert(testTeam);
        testTeam.setId(testid);
        testTeam.setName("postupdate");
        int updatedRows = teamDAO.update(testTeam);
        assertEquals(1, updatedRows);

        Optional<Team> dbTeam = teamDAO.getById(testid);
        assertFalse(dbTeam.isEmpty());
        assertEquals("postupdate", dbTeam.get().getName());
    }

    @Test
    void testUpdateFailsWhenInvalidBaton() {
        Baton testBaton = new Baton("testbaton");
        int batonId = batonDAO.insert(testBaton);
        Team testTeam = new Team("testteam", batonId);
        int teamId = teamDAO.insert(testTeam);
        // TODO: this is a little awkward, monitor
        // if this happens often in the real code
        // and find a better way
        testTeam.setId(teamId);

        testTeam.setBatonId(batonId + 1);
        assertThrows(UnableToExecuteStatementException.class,
                () -> teamDAO.update(testTeam));
    }

    @Test
    void updateDoesntDoAnythingWhenNotExists() {
        Team testTeam = new Team("test");
        int updatedRows = teamDAO.update(testTeam);
        List<Team> teams = teamDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(0, teams.size());
    }

    @Test
    void deleteRemovesTeam() {
        Team testTeam = new Team("test");
        int id = teamDAO.insert(testTeam);
        int updatedRows = teamDAO.deleteById(id);

        List<Team> teams = teamDAO.getAll();
        assertEquals(1, updatedRows);
        assertEquals(0, teams.size());
    }

    @Test
    void deleteDoesNothingIfNotExists() {
        Team testTeam = new Team("test");
        int id = teamDAO.insert(testTeam);
        int updatedRows = teamDAO.deleteById(id + 1);

        List<Team> teams = teamDAO.getAll();
        assertEquals(0, updatedRows);
        assertEquals(1, teams.size());
    }
}