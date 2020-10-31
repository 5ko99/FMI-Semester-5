package bg.sofia.uni.fmi.mjt.netflix.exceptions;

import java.util.NoSuchElementException;

public class UserNotFoundException extends NoSuchElementException {
    public  UserNotFoundException(String errorMsg){
        super(errorMsg);
    }
}
