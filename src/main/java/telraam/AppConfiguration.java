package telraam;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.dropwizard.Configuration;
import io.dropwizard.db.DataSourceFactory;
import telraam.api.responses.Template;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

public class AppConfiguration extends Configuration {
    @NotNull
    private String template;

    @NotNull
    private String defaultName = "Stranger";

    private int beaconPort;

    @Valid
    private List<ApplicationCredentialFactory> applicationCredentials;

    @Valid
    @NotNull
    private DataSourceFactory database = new DataSourceFactory();

    @JsonProperty
    public String getTemplate() {
        return template;
    }

    @JsonProperty
    public void setTemplate(String template) {
        this.template = template;
    }

    public Template buildTemplate() {
        return new Template(template, defaultName);
    }

    @JsonProperty
    public String getDefaultName() {
        return defaultName;
    }

    @JsonProperty
    public void setDefaultName(String name) {
        this.defaultName = name;
    }

    @JsonProperty("database")
    public DataSourceFactory getDataSourceFactory() {
        return database;
    }

    @JsonProperty("database")
    public void setDataSourceFactory(DataSourceFactory factory) {
        this.database = factory;
    }

    @JsonProperty("beaconPort")
    public int getBeaconPort() {
        return beaconPort;
    }

    @JsonProperty("beaconPort")
    public void setBeaconPort(int port) {
        beaconPort = port;
    }

    @JsonProperty("applicationCredentials")
    public List<ApplicationCredentialFactory> getApplicationCredentials() {
        return applicationCredentials;
    }

    @JsonProperty("applicationCredentials")
    public void setApplicationCredentials(List<ApplicationCredentialFactory> applicationCredentials) {
        this.applicationCredentials = applicationCredentials;
    }

    static class ApplicationCredentialFactory extends Configuration {
        String username;
        String password;

        @JsonProperty("username")
        public String getUsername() {
            return username;
        }

        @JsonProperty("username")
        public void setUsername(String username) {
            this.username = username;
        }

        @JsonProperty("password")
        public String getPassword() {
            return password;
        }

        @JsonProperty("password")
        public void setPassword(String password) {
            this.password = password;
        }

        @Override
        public String toString() {
            return "ApplicationCredentialFactory{" +
                    "username='" + username + '\'' +
                    ", password='" + password + '\'' +
                    '}';
        }
    }
}
