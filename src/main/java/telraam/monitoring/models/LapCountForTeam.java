package telraam.monitoring.models;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Map;

public class LapCountForTeam {
    @JsonProperty("team_name")
    private String teamName;
    @JsonProperty("lap_counts")
    private Map<Integer, Integer> lapCounts;

    public LapCountForTeam(String teamName, Map<Integer, Integer> lapCounts) {
        this.teamName = teamName;
        this.lapCounts = lapCounts;
    }
}
