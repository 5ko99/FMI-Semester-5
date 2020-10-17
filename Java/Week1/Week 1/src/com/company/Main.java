package com.company;
import java.lang.reflect.Array;
import java.util.Arrays;

import static com.company.SocialDistanceMaximizer.maxDistance;
import static com.company.SandwichExtractor.extractIngredients;
import static com.company.Remembrall.isPhoneNumberForgettable;
public class Main {

    public static void main(String[] args) {
        int a = maxDistance(new int[]{1, 0, 0, 0, 1, 0, 1}); //2
        int b = maxDistance(new int[]{1,0,0,0}); //3
        int c = maxDistance(new int[]{0, 1}); //1
        System.out.println(String.format("%d %d %d",a,b,c));
        System.out.println(Arrays.toString(extractIngredients("asdbreadham-tomato-mayobreadblabla")));
        System.out.println(Arrays.toString(extractIngredients("asdbreadham-olives-tomato-olives-mayobreadblabla")));
        System.out.println(Arrays.toString(extractIngredients("asdbreadham")));

        System.out.println(isPhoneNumberForgettable(""));//false
        System.out.println(isPhoneNumberForgettable("498-123-123"));//false
        System.out.println(isPhoneNumberForgettable("0894 123 567"));//true
        System.out.println(isPhoneNumberForgettable("(888)-FLOWERS"));//true
        System.out.println(isPhoneNumberForgettable("(444)-greens"));//true
    }


}

