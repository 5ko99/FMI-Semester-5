package bg.sofia.uni.fmi.mjt.netflix.exceptions;

import java.security.AccessControlException;

public class ContentUnavailableException extends AccessControlException {
    public ContentUnavailableException(String errMsg){
        super(errMsg);
    }
}
