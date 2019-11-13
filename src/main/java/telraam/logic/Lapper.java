package telraam.logic;

import telraam.beacon.BeaconMessage;

public interface Lapper {
    void handle(BeaconMessage msg);
}
