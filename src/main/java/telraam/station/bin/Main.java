package telraam.station.bin;

import telraam.station.Fetcher;

// Just run the fetched man
public class Main {
   public static void main(String[] args) {
      Fetcher fetcher = new Fetcher();
      fetcher.addStation("http://localhost:8001/detection/");
      fetcher.addStation("http://localhost:8002/detection/");
      fetcher.addStation("http://localhost:8003/detection/");
      fetcher.addStation("http://localhost:8004/detection/");
      fetcher.addDetectionHanlder(x -> System.out.println(x.getStationId() + " " + x.getId()));

      fetcher.start().run();
   }
}
