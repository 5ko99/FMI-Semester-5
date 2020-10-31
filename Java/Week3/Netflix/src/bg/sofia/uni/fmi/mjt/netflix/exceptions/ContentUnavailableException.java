package bg.sofia.uni.fmi.mjt.netflix.exceptions;

public class ContentUnavailableException extends Exception {
    public ContentUnavailableException(String errMsg){
        super(errMsg);
    }
}
