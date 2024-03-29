package telraam.monitoring.models;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;

import java.util.Map;

@AllArgsConstructor
public class LapCountForTeam {

    @JsonProperty("team_name")
    private String teamName;

    @JsonProperty("lap_counts")
    private Map<Integer, Integer> lapCounts;
}
