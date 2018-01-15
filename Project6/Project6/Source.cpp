#include <iostream>
    using namespace std;

   
      // return true if two C strings are equal
    bool strequal(const char str1[], const char str2[])
    {
        while (*str1 != 0  &&  *str2 != 0) //we add asteriks to actually access the values stored in the pointers
        {
            if (*str1 != *str2)  // compare corresponding characters
                return false;
            str1++;            // advance to the next character
            str2++;
        }
        return *str1 == *str2;   // both ended at same time?
    }

    int main()
    {
        char a[15] = "Zhao";
        char b[15] = "Zhou";

        if (strequal(a,b))
            cout << "They're the same person!\n";
    }
