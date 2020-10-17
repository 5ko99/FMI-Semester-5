package com.company;

import java.util.Arrays;

public class Remembrall {
    public static boolean isPhoneNumberForgettable(String phoneNumber) {
        if(phoneNumber.isEmpty()) return false;

        if(!phoneNumber.matches("^[0-9\\ \\-]+$")) {
            return true;
        }

        char[] spl = phoneNumber.toCharArray();
        Arrays.sort(spl);
        for(int i=0;i<spl.length-1;++i) {
            if(Character.isDigit(spl[i])&&spl[i]==spl[i+1]) return false;
        }
        return true;
    }
}
