package telraam.beacon;

/**
* Interface for callbacks with one parameter.
*
* @author  Arthur Vercruysse
* @param <O> The return value type
* @param <I> The type of the parameter
*/
public interface Callback<O, I> {
    public O handle(I value);
}
