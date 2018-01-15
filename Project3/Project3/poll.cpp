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

bool hasCorrectSyntax(string pollData) 
{
	int i = 0;
	while (i + 3 < pollData.size()) 
	{
		if (!isalpha(pollData[i]) || !isalpha(pollData[i + 1])) {
			cout << "case 1";
			return false;
		}
		string stateCode = "";
		stateCode += toupper(pollData[i]);
		stateCode += toupper(pollData[i + 1]);
		if (!isValidUppercaseStateCode(stateCode)) {
			cout << "case 2";
			return false;
		}
		if (!isdigit(pollData[i + 2])) 
		{
			cout << "case 3";
			return false;
		}
		if (!isdigit(pollData[i + 3]) && !isalpha(pollData[i + 3])) 
		{
			cout << "case 4";
			return false;
		}
		if (isdigit(pollData[i + 3])) 
		{
			if (i + 4 >= pollData.size()) {
				cout << "case 5";
				return false;
			}
			if (!isalpha(pollData[i + 4])) {
				cout << "case 6";
				return false;
			}
			i += 1;
		}
		i += 4;
		
	}
	return true;
}

int countVotes(string pollData, char party, int& voteCount) {
	if(!hasCorrectSyntax(pollData)) {
		return 1;
	}
	if (!isalpha(party)) {
		return 3;
	}
	int i = 0;
	while (i + 3 < pollData.size()) {
		if (isdigit(pollData[i + 3])) {
			voteCount = (pollData[i + 2] - '0') * 10 + (pollData[i + 3] - '0');
		}
		if (voteCount == 0) {
			return 2;
		}
	}
	return 0;
}

int main()
{
	string s = "";
	getline(cin, s);
	cout << hasCorrectSyntax(s) << endl;
	assert(hasCorrectSyntax("TX38RCA55D"));
	assert(!hasCorrectSyntax("MX38RCA55D"));
	int votes;
	votes = -999;    // so we can detect whether countVotes sets votes
	assert(countVotes("TX38RCA55DMs6rnY29dUT06L", 'd', votes) == 0  &&  votes == 84);
	votes = -999;    // so we can detect whether countVotes sets votes
	//assert(countVotes("TX38RCA55D", '%', votes) == 3  &&  votes == -999);
	//…
	//cout << "All tests succeeded" << endl;
}