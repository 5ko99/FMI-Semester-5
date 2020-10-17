package com.company;

import java.lang.reflect.Array;
import java.util.Arrays;

public class SocialDistanceMaximizer {
    public static int maxDistance(int[] seats) {
        int[] maxDist = new int[seats.length];
        for(int i=0;i<maxDist.length;++i) {
            if(seats[i]==0) maxDist[i] = Integer.MAX_VALUE;
            else maxDist[i] = Integer.MIN_VALUE;
        }
        for(int i=0;i<maxDist.length;++i) {
            if(seats[i]==1) continue;
            for(int j=0;j<maxDist.length;++j) {
                if(seats[j]==1) {
                    if(Math.abs(i - j) == 1) {
                        maxDist[i] = Math.min(1, maxDist[i]);
                        break;
                    }
                    else {
                        maxDist[i] = Math.min(Math.abs(i - j), maxDist[i]);
                    }
                }
            }
        }
        return getMax(maxDist);
    }

    private static int getMax(int[] arr){
        int max = arr[0];
        for(var elem : arr){
            if(elem>max) max = elem;
        }
        return max;
    }
}
