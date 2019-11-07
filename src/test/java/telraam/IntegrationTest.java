package telraam;

import io.dropwizard.testing.junit5.DropwizardExtensionsSupport;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(DropwizardExtensionsSupport.class)
public class IntegrationTest {

//    private static final String TMP_FILE = createTempFile();
//    private static final String CONFIG_PATH = ResourceHelpers.resourceFilePath("test-example.yml");
//
//    public static final DropwizardAppExtension<AppConfiguration> RULE = new DropwizardAppExtension<AppConfiguration>(
//            App.class, CONFIG_PATH,
//            ConfigOverride.config("database.url", "jdbc:h2:" + TMP_FILE));
//
//    @BeforeAll
//    public static void migrateDb() throws Exception {
//        RULE.getApplication().run("db", "migrate", CONFIG_PATH);
//    }
//
//    private static String createTempFile() {
//        try {
//            return File.createTempFile("test-example", null).getAbsolutePath();
//        } catch (IOException e) {
//            throw new IllegalStateException(e);
//        }
//    }
//
//    @Test
//    public void testHelloWorld() throws Exception {
//        final Optional<String> name = Optional.of("Dr. IntegrationTest");
//        final Saying saying = RULE.client().target("http://localhost:" + RULE.getLocalPort() + "/hello-world")
//                .queryParam("name", name.get())
//                .request()
//                .get(Saying.class);
//        assertEquals(RULE.getConfiguration().buildTemplate().render(name), saying.getContent());
//    }
//
//    @Test
//    public void testPostPerson() throws Exception {
//        final Baton baton = new Baton("Chief Wizard");
//        final Baton newBaton = postBaton(baton);
//        assertNotNull(newBaton.getId());
//        assertEquals(newBaton.getName(), baton.getName());
//    }
//
//    @Test
//    public void testRenderingPersonFreemarker() throws Exception {
//        testRenderingPerson("view_freemarker");
//    }
//
//    @Test
//    public void testRenderingPersonMustache() throws Exception {
//        testRenderingPerson("view_mustache");
//    }
//
//    private void testRenderingPerson(String viewName) throws Exception {
//        final Baton baton = new Baton("Chief Wizard");
//        final Baton newBaton = postBaton(baton);
//        final String url = "http://localhost:" + RULE.getLocalPort() + "/people/" + newBaton.getId() + "/" + viewName;
//        Response response = RULE.client().target(url).request().get();
//        assertEquals(HttpStatus.OK_200, response.getStatus());
//    }
//
//    private Baton postBaton(Baton baton) {
//        return RULE.client().target("http://localhost:" + RULE.getLocalPort() + "/baton")
//                .request()
//                .post(Entity.entity(baton, MediaType.APPLICATION_JSON_TYPE))
//                .readEntity(Baton.class);
//    }
//
//    @Test
//    public void testLogFileWritten() throws IOException {
//        // The log file is using a size and time based policy, which used to silently
//        // fail (and not write to a log file). This test ensures not only that the
//        // log file exists, but also contains the log line that jetty prints on startup
//        final Path log = Paths.get("./logs/application.log");
//        assertNotNull(log);
//        final String actual = new String(Files.readAllBytes(log), UTF_8);
//        assertTrue(actual.contains("0.0.0.0:" + RULE.getLocalPort()));
//    }
}