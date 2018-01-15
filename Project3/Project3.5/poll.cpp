#include <string>
#include <iostream>
#include <cctype>
#include <cassert>
using namespace std; 

// Return true if the argument is a two-uppercase-letter state code, or
// false otherwise.

bool isValidUppercaseStateCode(string stateCode)
{
    const string codes =
	"AL.AK.AZ.AR.CA.CO.CT.DE.DC.FL.GA.HI.ID.IL.IN.IA.KS."
        "KY.LA.ME.MD.MA.MI.MN.MS.MO.MT.NE.NV.NH.NJ.NM.NY.NC."
        "ND.OH.OK.OR.PA.RI.SC.SD.TN.TX.UT.VT.VA.WA.WV.WI.WY";
    return (stateCode.size() == 2  &&
	    stateCode.find('.') == string::npos  &&  // no '.' in stateCode
	    codes.find(stateCode) != string::npos);  // match found
}

//tests if the pollData string has correct syntax
bool hasCorrectSyntax(string pollData) 
{
	//initializes a variable for the while loop
	int i = 0;
	//while loop to process the data, want to check if the i + 3 is at least contained in the string
	while (i + 3 < pollData.size()) 
	{
		//test if the first two characters are letters
		if (!isalpha(pollData[i]) || !isalpha(pollData[i + 1])) {
			return false;
		}
		//put them in a string and test if they make a state code
		string stateCode = "";
		stateCode += toupper(pollData[i]);
		stateCode += toupper(pollData[i + 1]);
		if (!isValidUppercaseStateCode(stateCode)) {
			return false;
		}
		//test if the third character is a digit
		if (!isdigit(pollData[i + 2])) 
		{
			return false;
		}
		//test if the fourth character is either a letter or digit
		if (!isdigit(pollData[i + 3]) && !isalpha(pollData[i + 3])) 
		{
			return false;
		}
		//test if the fourth digit is a digit
		if (isdigit(pollData[i + 3])) 
		{
			//see if getting the fifth character would be out of bounds
			if (i + 4 >= pollData.size()) {
				return false;
			}
			//if not, test if that fifth character is a letter
			if (!isalpha(pollData[i + 4])) {
				return false;
			}
			//add 1 to i for iteration (since this is a five character prediction)
			i += 1;
		}
		//adds 4 to i
		i += 4;
		
	}
	//tests to see if there are still characters unprocessed, if there is, the syntax must be wrong
	if (pollData.size() - i > 0) {
		return false;
	}
	return true;
}

//counts the predicted votes for a given party
int countVotes(string pollData, char party, int& voteCount) {
	//tests if the string has correct syntax
	if(!hasCorrectSyntax(pollData)) {
		return 1;
	}
	//tests if the party is a letter
	if (!isalpha(party)) {
		return 3;
	}
	//initialize i for iteration
	int i = 0;
	//total for number of votes for the party (not using voteCount since one of the predictions might be 0, in which case
	//voteCount should remain unchanged)
	int total = 0;
	while (i + 3 < pollData.size()) {
		//temporary value used for the predicted votes
		int temp = 0;
		if (isdigit(pollData[i + 3])) {
			//if the 4th character is a digit, finds the number contained by 3rd and 4th characters
			temp += (pollData[i + 2] - '0') * 10 + (pollData[i + 3] - '0');
			//test to see if prediction is 0
			if (temp == 0) {
				return 2;
			}
			//finds if the parties are the same
			if (tolower(pollData[i + 4]) != party) {
				temp = 0;
			}
			//adds 1 to i for iteration
			i += 1;
		}
		else {
			//finds number contained in the third character
			temp += pollData[i + 2] - '0';
			//see above
			if (temp == 0) {
				return 2;
			}
			if (tolower(pollData[i + 3]) != party){
				temp = 0;
			}
		}
		//adds to total and iterates
		total += temp;
		i += 4;
	}
	//set voteCount to total if nothing went wrong
	voteCount = total;
	return 0;
}

int main()
{
	//my test cases
	/*
	assert(hasCorrectSyntax("TX38RCA55D"));
	assert(!hasCorrectSyntax("MX38RCA55D"));
	assert(hasCorrectSyntax(""));
	assert(hasCorrectSyntax("Az28dIL4rTx42gmt4RNY15d"));
	assert(!hasCorrectSyntax("XY42rCo9Dca10r"));
	assert(!hasCorrectSyntax("Az33gIl5rAzR"));
	assert(!hasCorrectSyntax("cA55dID4Az38R"));
	int votes;
	votes = -999;    // so we can detect whether countVotes sets votes
	assert(countVotes("TX38RCA55DMs6rnY29dUT06L", 'd', votes) == 0 && votes == 84);
	votes = -999;    // so we can detect whether countVotes sets votes
	assert(countVotes("TX38RCA55D", '%', votes) == 3 && votes == -999);
	votes = -999;
	int num = countVotes("cA55dID4rnY38R", 'd', votes);
	assert(votes == 55 && num == 0);
	votes = -999;
	num = countVotes("CX34rnYral9r", 'd', votes);
	assert(num == 1 && votes == -999);
	votes = -999;
	num = countVotes("Az0dNY43g", 'd', votes);
	assert(num == 2 && votes == -999);
	votes = -999;
	num = countVotes("Az32dID4rny18R", '%', votes);
	assert(num == 3 && votes == -999);
	votes = -999;
	num = countVotes("xy00rNyral9r", 'r', votes);
	assert(num == 1 && votes == -999);
	votes = -999;
	num = countVotes("Az35dID00rny28R", '%', votes);
	assert(num == 3 && votes == -999);
	votes = -999;
	num = countVotes("xy55rnyral9r", '&', votes);
	assert(num == 1 && votes == -999);
	votes = -999;
	num = countVotes("xY55rNyral00r", '%', votes);
	assert(num == 1 && votes == -999);
	votes = -999;
	num = countVotes("CO9DCa55d", 'R', votes);
	assert(num == 0 && votes == 0);
	cout << "all tests succeeded" << endl;
	*/
}