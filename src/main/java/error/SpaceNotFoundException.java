package error;

public class SpaceNotFoundException  extends RuntimeException{

    public SpaceNotFoundException() {
    }

    public SpaceNotFoundException(String message) {
        super(message);
    }
}
