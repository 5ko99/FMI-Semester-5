package bg.sofia.uni.fmi.mjt.netflix.exceptions;

import java.util.NoSuchElementException;

public class ContentNotFoundException extends NoSuchElementException {
    public  ContentNotFoundException(String errorMsg){
        super(errorMsg);
    }
}
