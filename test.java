package test;
import java.util.*;
public class add {
    int x = 0;
    public static void main(int args) {
        System.out.println("Hello World");
        
        int length = 5, breadth = -6;

        do{
            length++;
            {
                int a = 0,b=1;
                int x = 5;
                a = b*x + 1;
                System.out.println("Inside Dowhile");
            }

            b += 1;
            b -= 5;
            for (int x = 2; x <= 4;) 
                System.out.println(x); 
        }while(length!=0);
        
    }
}