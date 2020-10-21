//package com.company;

import java.lang.reflect.Array;
import java.util.Arrays;

public class SandwichExtractor {
    public static String[] extractIngredients(String sandwich) {
        final String br = "bread";
        final String sep = "-";
        final int brN = 5;
        final int start = sandwich.indexOf(br);
        final int end = sandwich.indexOf(br,start+1);
        final String olives = "olives";

        if(start==-1 || end==-1){
            return new String[]{};
        }
        String[] temp = sandwich.subSequence(start+brN,end).toString().split(sep);

        //Remove Olives
        String[] res = new String[temp.length];
        int j = 0;
        for(int i=0;i<temp.length;++i){
            if(!(temp[i].equals(olives))) res[j++] = temp[i];
        }
        res = Arrays.copyOf(res,j);
        return res;
    }
}
