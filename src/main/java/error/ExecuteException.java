package error;

public class ExecuteException extends RuntimeException{

    public ExecuteException() {
    }

    public ExecuteException(String message) {
        super(message);
    }
}
