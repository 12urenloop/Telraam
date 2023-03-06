package telraam.api;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import telraam.DatabaseTest;
import telraam.database.daos.BatonDAO;
import telraam.database.models.Baton;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import javax.ws.rs.WebApplicationException;

import static org.junit.jupiter.api.Assertions.*;

public class BatonResourceTest extends DatabaseTest {

    private BatonDAO batonDAO;

    @Override
    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        batonDAO = jdbi.onDemand(BatonDAO.class);
    }

    @Test
    public void testGetAll() {
        var baton1 = new Baton("Foccacia", "mac1");
        var baton2 = new Baton("Glutenvrij", "mac2");

        baton1.setId(batonDAO.insert(baton1));
        baton2.setId(batonDAO.insert(baton2));

        var resource = new BatonResource(batonDAO);
        var result = resource.getListOf();

        assertEquals(2, result.size());
        assertEquals(Set.of(baton1, baton2), new HashSet<>(result));
    }

    @Test
    public void testGetFirst() {
        var baton1 = new Baton("Foccacia", "mac1");
        var baton2 = new Baton("Glutenvrij", "mac2");

        baton1.setId(batonDAO.insert(baton1));
        baton2.setId(batonDAO.insert(baton2));

        var resource = new BatonResource(batonDAO);
        var result = resource.get(Optional.of(baton1.getId()));

        assertEquals(result, baton1);
    }

    @Test
    public void testGetNoId() {
        var resource = new BatonResource(batonDAO);
        assertThrows(WebApplicationException.class, () -> resource.get(Optional.empty()));
    }

    @Test
    public void testGetNonExisting() {
        var resource = new BatonResource(batonDAO);
        assertThrows(WebApplicationException.class, () -> resource.get(Optional.of(1)));
    }

    @Test
    public void testCreate() {
        var baton1 = new Baton("Foccacia", "mac1");

        var resource = new BatonResource(batonDAO);
        baton1.setId(resource.create(baton1));
        
        assertEquals(batonDAO.getById(baton1.getId()).get(), baton1);
    }

    @Test
    public void testDelete() {
        var baton1 = new Baton("Foccacia", "mac1");
        var baton2 = new Baton("Glutenvrij", "mac2");

        baton1.setId(batonDAO.insert(baton1));
        baton2.setId(batonDAO.insert(baton2));

        var resource = new BatonResource(batonDAO);
        assert(resource.delete(Optional.of(baton1.getId())));
        assert(!resource.delete(Optional.of(baton1.getId())));

        assertEquals(1, batonDAO.getAll().size());
    }

    @Test
    public void testDeleteNoId() {
        var baton1 = new Baton("Foccacia", "mac1");
        var baton2 = new Baton("Glutenvrij", "mac2");

        baton1.setId(batonDAO.insert(baton1));
        baton2.setId(batonDAO.insert(baton2));

        var resource = new BatonResource(batonDAO);
        assertThrows(WebApplicationException.class, () -> resource.delete(Optional.empty()));

        assertEquals(2, batonDAO.getAll().size());
    }

    @Test
    public void testUpdate() {
        var baton1 = new Baton("Foccacia", "mac1");
        var update = new Baton("Glutenvrij", "mac2");

        baton1.setId(batonDAO.insert(baton1));

        var resource = new BatonResource(batonDAO);
        var updated = resource.update(update, Optional.of(baton1.getId()));

        var baton2 = batonDAO.getById(baton1.getId()).get();
        assertEquals(baton2.getName(), updated.getName());
    }

    @Test
    public void testUpdateNoId() {
        var update = new Baton("Glutenvrij", "mac1");
        var resource = new BatonResource(batonDAO);
        assertThrows(WebApplicationException.class, () -> resource.update(update, Optional.empty()));
    }

    @Test
    public void testUpdateNonExistentId() {
        var baton1 = new Baton("Foccacia", "mac1");
        var update = new Baton("Glutenvrij", "mac2");

        baton1.setId(batonDAO.insert(baton1));

        var resource = new BatonResource(batonDAO);
        assertThrows(WebApplicationException.class, () -> resource.update(update, Optional.of(baton1.getId() + 1)));
    }
}
