package telraam.logic.plapper;

import telraam.database.models.BatonSwitchover;
import telraam.database.models.Detection;

import java.lang.reflect.Field;
import java.util.List;

public class PSerializer {
    public static String serialize(Detection detection) {
        return new StringBuilder()
                .append("detection(")
                .append(detection.getId()).append(",")
                .append(detection.getRssi())
                .append(")").toString();
    }

    public static String serialize(BatonSwitchover batonSwitchover) {
        return "baton_switchover(" + batonSwitchover.getId() + ","
                + batonSwitchover.getTeamId() + ","
                + batonSwitchover.getPreviousBatonId() + ","
                + batonSwitchover.getNewBatonId() + ","
                + batonSwitchover.getId() + ",";
    }

    public static String serialize(List<String> compounds) {
        return "[" + String.join(",", compounds) + "]";
    }
}
