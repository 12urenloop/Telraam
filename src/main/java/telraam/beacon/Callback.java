package telraam.beacon;

/**
* Stupid interface for callbacks. You mind if I request Callback<Void, Void>?
*
* @author  Arthur Vercruysse
*/
public interface Callback<Output, Input> {
    public Output handle(Input value);
}
