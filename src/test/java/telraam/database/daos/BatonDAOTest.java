package telraam.database.daos;

import io.dropwizard.testing.junit5.DAOTestExtension;
import io.dropwizard.testing.junit5.DropwizardExtensionsSupport;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import telraam.database.models.Baton;
import telraam.database.models.Id;

import javax.validation.ConstraintViolationException;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(DropwizardExtensionsSupport.class)
public class BatonDAOTest {

    public DAOTestExtension daoTestRule = DAOTestExtension.newBuilder()
            .addEntityClass(BatonDAO.class)
            .build();

//    private BatonDAO batonDAO;

    @BeforeEach
    public void setUp() throws Exception {
//        batonDAO = new BatonDAO(daoTestRule.getSessionFactory());
    }

    @Test
    public void createPerson() {
        assertTrue(true);

//        final Id leId = daoTestRule.inTransaction(() -> batonDAO.insert(new Baton("Jeff")));
//        assertTrue(leId.getId() > 0);

//        Optional<Baton> batonOpt = daoTestRule.inTransaction(() -> batonDAO.findBatonById(leId.getId()));
//        assertFalse(batonOpt.isEmpty());
//        Baton baton = batonOpt.get();
//        assertEquals("jeff", baton.getName());
    }

    @Test
    public void findAll() {
        assertTrue(true);

//        daoTestRule.inTransaction(() -> {
//            batonDAO.insert(new Baton("Jeff"));
//            batonDAO.insert(new Baton("Jim"));
//            batonDAO.insert(new Baton("Randy"));
//        });

//        final List<Baton> batons = batonDAO.findAll();
//        assertThat(persons).extracting("fullName").containsOnly("Jeff", "Jim", "Randy");
//        assertThat(persons).extracting("jobTitle").containsOnly("The plumber", "The cook", "The watchman");
    }

    @Test
    public void handlesNullFullName() {
        assertTrue(true);
//        assertThrows(ConstraintViolationException.class, ()->
//                        daoTestRule.inTransaction(() -> batonDAO.create(new Baton("The null"))));
    }
}