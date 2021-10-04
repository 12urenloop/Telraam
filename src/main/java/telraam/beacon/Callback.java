package telraam.beacon;

/**
 * Interface for callbacks with one parameter.
 *
 * @param <O> The return value type
 * @param <I> The type of the parameter
 * @author Arthur Vercruysse
 */
public interface Callback<O, I> {
    public O handle(I value);
}
