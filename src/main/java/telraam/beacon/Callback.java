package telraam.beacon;

public interface Callback<Output, Input> {
    public Output handle(Input value);
}
