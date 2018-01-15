	#include <iostream>
	#include <string>
	#include <cassert>
	using namespace std;

	//adds the value to each element in the array
	int appendToAll(string a[], int n, string value) {
		//tests to see if n is less than 0, almost the same for each function
		if (n < 0) {
			return -1;
		}
		//for loop to add the value to each element
		for (int i = 0; i < n; i++) {
			a[i] += value;
		}
		return n;
	}

	//sees if a certain item is in the array
	int lookup(const string a[], int n, string target) {
		if (n < 0) {
			return -1;
		}
		//finds which position the item is at using for loop; if the item isn't in the array, returns -1
		int position = -1;
		for (int i = 0; i < n; i++) {
			if (a[i] == target) {
				position = i;
			}
		}
		return position;
	}

	//finds the highest valued string in the array
	int positionOfMax(const string a[], int n) {
		//if there are non-positive elements, return -1
		if (n <= 0) {
			return -1;
		}
		//returns position of the max string
		int maxString = 0;
		//starts at 2nd element and compares it to the first
		for (int i = 1; i < n; i++) {
			if (a[i] > a[maxString]) {
				maxString = i;
			}
		
		}
		return maxString;
	}

	//takes one element and puts it at the end, rotating everything else left in the process
	int rotateLeft(string a[], int n, int pos) {
		//if pos is out of bounds, pos is negative, n is less than 0, returns -1
		if (n <= 0 || pos >= n || pos < 0) {
			return -1;
		}
		//stores the item at pos in a string
		string temp = a[pos];
		//for loop to shift everything to the right
		for (int i = pos + 1; i < n; i++) {
			a[i - 1] = a[i];
		}
		//places item at pos at then end of the array and returns original pos
		a[n - 1] = temp;
		return pos;
	}

	//counts the number of sequences
	int countRuns(const string a[], int n) {
		if (n < 0) {
			return -1;
		}
		//0 runs if n is 0
		if (n == 0) {
			return 0;
		}
		//for loop to see if the consecutive elements are identical, if not, increments
		int count = 1;
		for (int i = 1; i < n; i++) {
			if (a[i] != a[i - 1]) {
				count++;
			}
		}
		//returns number of sequences
		return count;
	}

	//flips the elements in the array
	int flip(string a[], int n) {
		if (n < 0) {
			return -1;
		}
		//iterates through first half of loop (so that it doesn't reflip the array which would end up
		//with the original array)
		for (int i = 0; i < (n / 2); i++) {
			//swaps elements to reverse the array
			string temp = a[(n - 1) - i];
			a[(n - 1) - i] = a[i];
			a[i] = temp;
		}
		return n;
	}

	//returns first position where the two elements in the two different arrays are not equal
	int differ(const string a1[], int n1, const string a2[], int n2) {
		//sees if either of the n's are less than 0
		if (n1 < 0 || n2 < 0) {
			return -1;
		}
		//integer i used for incrementation
		int i = 0;
		//while loop to compare the elements in the array
		while (i < n1 && i < n2) {
			if (a1[i] != a2[i]) {
				//if they are different, returns this position
				return i;
			}
			i++;
		}
		//if the arrays are equal to point where either runs out, return the smaller n
		if (n1 <= n2) {
			return n1;
		}
		else {
			return n2;
		}
	}

	//tests to see if second array is contained in the first one
	int subsequence(const string a1[], int n1, const string a2[], int n2) {
		if (n1 < 0 || n2 < 0) {
			return -1;
		}
		//returns 0 if the subsequence has 0 elements
		if (n2 == 0) {
			return 0;
		}
		//uses i for iteration
		int i = 0;
		while (i < n1) {
			//keeps iterating until it finds an element that is the same as the 2nd array's first element
			if (a1[i] == a2[0]) {
				//records this original position to potentially return later
				int originalPosition = i;
				//uses j for iteration (starts at 2nd element since 1st one has been checked
				int j = 1;
				//while loop to see if subsequence is in the array
				while (j < n2) {
					//increment i for more comparisons
					i++;
					//if i is out of bounds, it breaks out of while loop
					if(i >= n1){
						j = n2 + 1;
					}
					//if the elements don't match up, break out
					if (a1[i] != a2[j]) {
						j = n2 + 1;
						//decrement i because i gets incremented later, want to see if
						//next element could possibly start the subsequence
						i--;
					}
					//increments i for the loop
					j++;
				}
				//if we successfully went through the while loop without problems,
				//that means j will be equal to n2 so we can return the original position
				//that marks the start of the substring
				if (j == n2){
					return originalPosition;
				}
			}
			//incrementation of i
			i++;
		}
		//returns -1 if the subsequence was not found in the array
		return -1;
	}

	//finds if any elements correspond
	int lookupAny(const string a1[], int n1, const string a2[], int n2) {
		//if the n's are non-positive, return -1 because then no elements are common between the arrays
		if (n1 <= 0 || n2 <= 0) {
			return -1;
		}
		//double for loop to see if any of the elements are equal
		for (int i = 0; i < n1; i++) {
			for (int j = 0; j < n2; j++) {
				if (a1[i] == a2[j]) {
					//if there is a common one, return the position of the first one in the first array
					return i;
				}
			}
		}
		//if it's not found, return -1
		return -1;
	}

	//orders the array with regards to the separator, elements less than separator go in front
	//and elements greater than or equal to the separator are after that
	int separate(string a[], int n, string separator) {
		if (n < 0) {
			return -1;
		}
		//set position for the elements less than separator to go
		int position = 0;
		//for loop to sort the array
		for (int i = 0; i < n; i++) {
			//if it is less than seperator...
			if (a[i] < separator) {
				//moves it to the front of array by swapping the elemnents (similar to flip)
				string temp = a[i];
				a[i] = a[position];
				a[position] = temp;
				//increment position to give new locations for the elements less than separator
				//to go to
				position++;
			}
		}
		//then finds first element greater than or equal to separator and returns it
		for (int i = 0; i < n; i++) {
			if (a[i] >= separator) {
				return i;
			}
		}
		//if there are none, returns n
		return n;
	}

	int main()
	{
	    string h[7] = { "jill", "hillary", "donald", "tim", "", "evan", "gary" };
	    assert(lookup(h, 7, "evan") == 5);
	    assert(lookup(h, 7, "donald") == 2);
	    assert(lookup(h, 2, "donald") == -1);
	    assert(positionOfMax(h, 7) == 3);

	    string g[4] = { "jill", "hillary", "gary", "mindy" };
	    assert(differ(h, 4, g, 4) == 2);
	    assert(appendToAll(g, 4, "?") == 4 && g[0] == "jill?" && g[3] == "mindy?");
	    assert(rotateLeft(g, 4, 1) == 1 && g[1] == "gary?" && g[3] == "hillary?");

	    string e[4] = { "donald", "tim", "", "evan" };
	    assert(subsequence(h, 7, e, 4) == 2);

	    string d[5] = { "hillary", "hillary", "hillary", "tim", "tim" };
	    assert(countRuns(d, 5) == 2);
	
	    string f[3] = { "gary", "donald", "mike" };
	    assert(lookupAny(h, 7, f, 3) == 2);
	    assert(flip(f, 3) == 3 && f[0] == "mike" && f[2] == "gary");
	
	    assert(separate(h, 7, "gary") == 3);
	
	    cout << "All tests succeeded" << endl;
	}