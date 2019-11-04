package telraam.database;

public class DataAccessException extends Exception {

    /*
        Custom exception class to show message of all database access errors
     */

    public DataAccessException(String message, Throwable th) {
        super(message, th);
    }

}