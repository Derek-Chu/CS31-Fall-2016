#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <cstring>
#include <cassert>
#include <cctype>
using namespace std;

//max length of the rule words
 const int MAX_WORD_LENGTH = 20;

int normalizeRules(char word1[][MAX_WORD_LENGTH + 1], char word2[][MAX_WORD_LENGTH + 1], int distance[], int nRules)
{
	//if the number of elements are less than or equal to 0, return 0
	if (nRules <= 0) {
		return 0;
	}
	//iterates to find if it needs to remove a rule
	for (int i = 0; i < nRules; i++) {
		bool remove = false;
		//removes it if there are no characters in a word
		if (strlen(word1[i]) == 0 || strlen(word2[i]) == 0){
			remove = true;
		}
		//removes it if the distance is less than or equal to 0
		else if (distance[i] <= 0) {
			remove = true;
		}
		int j = 0;
		//removes rule if there are nonalphabetic characters, also changes each character to lowercase
		while (word1[i][j] != '\0') {
			if (!isalpha(word1[i][j])) {
				remove = true;
			}
			word1[i][j] = tolower(word1[i][j]);
			j++;
		}
		j = 0;
		//same for word 2
		while (word2[i][j] != '\0') {
			if (!isalpha(word2[i][j])) {
				remove = true;
			}
			word2[i][j] = tolower(word2[i][j]);
			j++;
		}
		//formula to remove the rule if it needs removing
		if (remove) {
			for (int j = i; j < nRules - 1; j++) {
				strcpy(word1[j], word1[j + 1]);
				strcpy(word2[j], word2[j + 1]);
				distance[j] = distance[j + 1];
			}
			//decrements rules by 1 and decrements i by 1 to parse the new rule that is in the removed rule's position
			i--;
			nRules--;
		}
	}
	for (int i = 0; i < nRules; i++) {
		bool remove1 = false;
		bool remove2 = false;
		//compares rules to see if they have the same rule, if they do, removes the one with lowest distance
		for (int j = i + 1; j < nRules; j++) {
			//compares first two words, then second two words
			if (strcmp(word1[i], word1[j]) == 0) {
				if (strcmp(word2[i], word2[j]) == 0) {
					//finds which one to remove
					if (distance[i] <= distance[j]) {
						remove1 = true;
					}
					else {
						remove2 = true;
					}
				}
			}
			//compares the words alternately (to see if both words are the same)
			if (strcmp(word1[i], word2[j]) == 0) {
				if (strcmp(word2[i], word1[j]) == 0) {
					if (distance[i] <= distance[j]) {
						remove1 = true;
					}
					else {
						remove2 = true;
					}
				}
			}
			//formula to remove second instance of the same rule
			if (remove2) {
				for (int k = j; k < nRules - 1; k++) {
					strcpy(word1[k], word1[k + 1]);
					strcpy(word2[k], word2[k + 1]);
					distance[k] = distance[k + 1];
				}
				j--;
				nRules--;
				remove2 = false;
			}
		}
		//formula to remove first instance of the same rule
		if (remove1) {
			for (int j = i; j < nRules - 1; j++) {
				strcpy(word1[j], word1[j + 1]);
				strcpy(word2[j], word2[j + 1]);
				distance[j] = distance[j + 1];
			}
			i--;
			nRules--;
		}
		
	}
	//returns final number of rules in the normal form
	return nRules;
}

int calculateSatisfaction(const char word1[][MAX_WORD_LENGTH + 1], const char word2[][MAX_WORD_LENGTH + 1], const int distance[], int nRules, const char document[])
{
	//if rules is less than or equal to 0, returns 0
	if (nRules <= 0) {
		return 0;
	}
	int satisfaction = 0;
	//formats document to lowercase all uppercase letters and get rid of characters that are not alphabetic or space characters
	char normalDocument[200];
	int j = 0;
	//formula to normalize the document
	for (int i = 0; document[i] != '\0'; i++) {
		if (isalpha(document[i]) || document[i] == ' ') {
			normalDocument[j] = tolower(document[i]);
			j++;
		}
	}
	//sets final value of the document to a 0 byte
	normalDocument[j] = 0;
	//makes double array for words in the document
	char normalWords[101][201];
	//variables for incrementation
	int k = 0;
	int count = 0;
	int otherCount = 0;
	while (normalDocument[otherCount] != '\0') {
		if (normalDocument[otherCount] != ' ') {
			//if the character is not a space character, puts it into one of the C-strings
			normalWords[k][count] = normalDocument[otherCount];
			count++;
			otherCount++;
		}
		else {
			//goes to next C-string if there is a space character and sets the last character of the C-string as a 0 byte
			normalWords[k][count] = 0;
			count = 0;
			k++;
			//keeps going through the normal document if the character is still a space character
			while (normalDocument[otherCount] == ' ') {
				otherCount++;
			}
		}
	}
	//sets final C-string's last character to a zero byte
	normalWords[k][count] = 0;
	//variable for the number of words in this double array
	int maxWords = k + 1;
	//for loops to find if a rule is satisfied
	for (int i = 0; i < nRules; i++) {
		//keeps going through each word until it finds a match
		for (int j = 0; j < maxWords; j++) {
			//match for the first rule word
			if (strcmp(word1[i], normalWords[j]) == 0) {
				//variable to see if rule is satisfied to move on to the next rule
				bool getOut = false;
				//variables to see if the travelling exceeds the limit (can't go past last word and can't go past
				//a certain distance)
				int travel = j + 1;
				int travelDistance = 1;
				//sees if the next word is after the first word
				while (travel < maxWords && travelDistance <= distance[i]) {
					if (strcmp(word2[i], normalWords[travel]) == 0) {
						//if it is satisfied, get out of searching for this rule plus increase satisfaction score
						getOut = true;
						satisfaction++;
						travel = maxWords;
						j = maxWords;
					}
					//increment to keep looking
					travel++;
					travelDistance++;
				}
				//if it hasn't been satisfied, look to see if the next word is before the first one
				if (!getOut) {
					//looks at words before the first one
					travel = j - 1;
					travelDistance = 1;
					while (travel >= 0 && travelDistance <= distance[i]) {
						if (strcmp(word2[i], normalWords[travel]) == 0) {
							//if satisfied, get out of searching for this rule
							satisfaction++;
							travel = -1;
							j = maxWords;
						}
						//variable updates to keep looking
						travel--;
						travelDistance++;
					}
				}
			}
			//the same thing, except we found the second word first and now looking for the first word
			else if (strcmp(word2[i], normalWords[i]) == 0) {
				//variable to see if rule is satisfied to move on to the next rule
				bool getOut = false;
				//variables to see if the travelling exceeds the limit (can't go past last word and can't go past
				//a certain distance)
				int travel = j + 1;
				int travelDistance = 1;
				//sees if the first word is after the second word
				while (travel < maxWords && travelDistance <= distance[i]) {
					if (strcmp(word1[i], normalWords[travel]) == 0) {
						//if it is satisfied, get out of searching for this rule plus increase satisfaction score
						getOut = true;
						satisfaction++;
						travel = maxWords;
						j = maxWords;
					}
					//increment to keep looking
					travel++;
					travelDistance++;
				}
				//if it hasn't been satisfied, look to see if the first word is before the second one
				if (!getOut) {
					travel = j - 1;
					travelDistance = 1;
					while (travel >= 0 && travelDistance <= distance[i]) {
						//if satisfied, get out of searching for this rule
						if (strcmp(word1[i], normalWords[travel]) == 0) {
							satisfaction++;
							travel = -1;
							j = maxWords;
						}
						//variable updates to keep looking
						travel--;
						travelDistance++;
					}
				}
			}
		}
	}
	return satisfaction;
}

//main method for testing whether this works or not
int main()
{
	const int TEST1_NRULES = 4;
	char test1w1[TEST1_NRULES][MAX_WORD_LENGTH + 1] = {
		"mad",       "deranged", "nefarious", "have"
	};
	char test1w2[TEST1_NRULES][MAX_WORD_LENGTH + 1] = {
		"scientist", "robot",    "plot",      "mad"
	};
	int test1dist[TEST1_NRULES] = {
		2,           4,          1,           13
	};
	assert(calculateSatisfaction(test1w1, test1w2, test1dist, TEST1_NRULES,
		"The mad UCLA scientist unleashed a deranged evil giant robot.") == 2);
	
	assert(calculateSatisfaction(test1w1, test1w2, test1dist, TEST1_NRULES,
		"The mad UCLA scientist unleashed    a deranged robot.") == 2);
	assert(calculateSatisfaction(test1w1, test1w2, test1dist, TEST1_NRULES,
		"**** 2016 ****") == 0);
	assert(calculateSatisfaction(test1w1, test1w2, test1dist, TEST1_NRULES,
		"  That plot: NEFARIOUS!") == 1);
	assert(calculateSatisfaction(test1w1, test1w2, test1dist, TEST1_NRULES,
		"deranged deranged robot deranged robot robot") == 1);
	assert(calculateSatisfaction(test1w1, test1w2, test1dist, TEST1_NRULES,
		"That scientist said two mad scientists suffer from deranged-robot fever.") == 0);
	const int NUMBER_OF_RULES = 10;
char word1[NUMBER_OF_RULES][MAX_WORD_LENGTH + 1] = { "hello", "my", "n-ame", "is", "hello", "yeon", "taek", "man", "girl", "" };
char word2[NUMBER_OF_RULES][MAX_WORD_LENGTH + 1] = { "yellow", "man", "is", "smallberg", "yellow", "water", "taek", "my", "bo y", ""};
int dist[NUMBER_OF_RULES] = {3, 2, 1, 4, 2, -2, 0, 2, 5, 10};



assert(calculateSatisfaction(word1, word2, dist, 4, "smallberg is my friend") == 1); //Test for correctness
assert(calculateSatisfaction(word1, word2, dist, 4, "hello smallberg is my yellow friend") == 1); //Test for correctness
assert(calculateSatisfaction(word1, word2, dist, 4, "hello smallberg is yellow friend") == 2); //Test for correctness

assert(calculateSatisfaction(word1, word2, dist, 4, "hello yellow hello yellow hello yellow") == 1); //Rule is only counted once
assert(calculateSatisfaction(word1, word2, dist, 4, "small!berg! @@@@#is!! my!! friend!!") == 1); //Non-valid characters are removed correctly
assert(calculateSatisfaction(word1, word2, dist, 4, "my       man     is     smallberg") == 2); //Multiple spaces do not affect outcome

assert(calculateSatisfaction(word1, word2, dist, 4, "my man, how are you") == 1); //Distance of 1 after should satisfy
assert(calculateSatisfaction(word1, word2, dist, 4, "my z man, how are you") == 1); //Distance of 2 after should satisfy
assert(calculateSatisfaction(word1, word2, dist, 4, "my z z man, how are you") == 0); //Distance of 3 after does not satisfy

assert(calculateSatisfaction(word1, word2, dist, 4, "man, my friendhow are you") == 1); //Distance of 1 before should satisfy
assert(calculateSatisfaction(word1, word2, dist, 4, "man, z my how are you") == 1); //Distance of 2 before should satisfy
assert(calculateSatisfaction(word1, word2, dist, 4, "man, z z my how are you") == 0); //Distance of 3 before does not satisfy

assert(normalizeRules(word1, word2, dist, NUMBER_OF_RULES) == 3);
	cout << "All tests have succeded" << endl;
}