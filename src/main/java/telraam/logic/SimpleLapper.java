package telraam.logic;

import telraam.beacon.BeaconMessage;
import telraam.database.models.Team;

import java.util.ArrayList;
import java.util.List;

public class SimpleLapper implements Lapper {
    private List<Team> teams;
    public SimpleLapper() {
    }

    @Override
    public void handle(BeaconMessage msg) {


    }
}
