package telraam.beacon;

/**
* Interface for callbacks with one parameter.
*
* @author  Arthur Vercruysse
* @param <Output> The return value type
* @param <Input> The type of the parameter
*/
public interface Callback<Output, Input> {
    public Output handle(Input value);
}
