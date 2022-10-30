package telraam.logic.baumwelch;

import io.dropwizard.jersey.setup.JerseyEnvironment;
import org.jdbi.v3.core.Jdbi;
import telraam.database.models.Detection;
import telraam.logic.Lapper;

import java.util.List;
import java.util.Map;

public class BaumWelchLapper implements Lapper {

    private final Thread runner = new Thread(this::run);
    private final Jdbi jdbi;

    public BaumWelchLapper(Jdbi jdbi) {
        this.jdbi = jdbi;

        //run();
        runner.start();
    }

    private void run() {
        HMM<Integer, Integer> hmm = new HMM<>(List.of(1, 2, 3, 4), List.of(1, 2, 3));
        System.out.println(hmm.getTransitionProbabilities());
        System.out.println(hmm.getEmissionProbabilities());

        var observations = List.of(1, 2, 3, 3, 3, 3, 3, 3, 3, 1);
        var forward = hmm.forward(observations, Map.of(1,0.25, 2,0.25, 3,0.25, 4,0.25), 1);
        var backward = hmm.backward(observations, Map.of(1,0.25, 2,0.25, 3,0.25, 4,0.25), 1);

        System.out.println(forward);
        System.out.println(backward);
        System.out.println(hmm.gammaProbabilities(observations, forward, backward));
        System.out.println(hmm.xiProbabilities(observations, forward, backward));

        System.out.println("==>");
        hmm.baumWelch(observations, Map.of(1,0.25, 2,0.25, 3,0.25, 4,0.25), 1);
        System.out.println(hmm.getTransitionProbabilities());
        System.out.println(hmm.getEmissionProbabilities());

        System.out.println("==>");
        hmm.baumWelch(observations, Map.of(1,0.25, 2,0.25, 3,0.25, 4,0.25), 1);
        System.out.println(hmm.getTransitionProbabilities());
        System.out.println(hmm.getEmissionProbabilities());

        System.out.println("==>");
        hmm.baumWelch(observations, Map.of(1,0.25, 2,0.25, 3,0.25, 4,0.25), 1);
        System.out.println(hmm.getTransitionProbabilities());
        System.out.println(hmm.getEmissionProbabilities());

        System.out.println("==>");
        hmm.baumWelch(observations, Map.of(1,0.25, 2,0.25, 3,0.25, 4,0.25), 1);
        System.out.println(hmm.getTransitionProbabilities());
        System.out.println(hmm.getEmissionProbabilities());

        System.out.println("==>");
        hmm.baumWelch(observations, Map.of(1,0.25, 2,0.25, 3,0.25, 4,0.25), 1);
        System.out.println(hmm.getTransitionProbabilities());
        System.out.println(hmm.getEmissionProbabilities());
    }


    @Override
    public void handle(Detection msg) {
        if (!runner.isAlive()) {
            runner.start();
        }
    }

    @Override
    public void registerAPI(JerseyEnvironment jersey) {
    }
}
