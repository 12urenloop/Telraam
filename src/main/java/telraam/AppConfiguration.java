package telraam;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.dropwizard.core.Configuration;
import io.dropwizard.db.DataSourceFactory;
import io.federecio.dropwizard.swagger.SwaggerBundleConfiguration;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import lombok.Getter;
import lombok.Setter;
import telraam.api.responses.Template;

public class AppConfiguration extends Configuration {
    @NotNull
    @Getter @Setter
    private String template;

    @NotNull
    @Getter @Setter
    private String defaultName = "Stranger";

    @JsonProperty("swagger")
    public SwaggerBundleConfiguration swaggerBundleConfiguration;

    @Valid
    @NotNull
    @Getter @Setter
    @JsonProperty("database")
    private DataSourceFactory dataSourceFactory = new DataSourceFactory();
}
