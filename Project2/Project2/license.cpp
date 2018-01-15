#include <iostream>
#include <string>
using namespace std;

int main()
{
	//declaring variables for later use
	string company;
	double revenue;
	string country;
	double licenseFee = 0;

	//these are fixed rates used to calculate the license fee
	const double BASE_RATE = 0.181;
	const double NORMAL_SECONDARY_RATE = 0.203;
	const double SPECIAL_SECONDARY_RATE = 0.217;
	const double TERTIARY_RATE = 0.23;

	//asks user for information
	cout << "Identification: ";
	getline(cin, company);
	cout << "Expected revenue (in millions): ";
	cin >> revenue;
	cin.ignore(10000, '\n');
	cout << "Country: ";
	getline(cin, country);
	cout << "---" << endl;

	//error cases if user has weird inputs
	if (company == "") 
	{
		cout << "You must enter a property identification." << endl;
		return 1;
	}
	else if (revenue < 0) 
	{
		cout << "The expected revenue must be nonnegative." << endl;
		return 1;
	}
	else if (country == "") 
	{
		cout << "You must enter a country." << endl;
		return 1;
	}
	//if nothing is wrong, calculates the license fee for the company
	else 
	{
		//this is used to get print out the license fee with three decimal points of accuracy
		cout.setf(ios::fixed);
		cout.precision(3);
		//finds which case this client belongs to and calculates the license fee accordingly
		if (revenue < 20)
		{
			licenseFee = revenue * BASE_RATE;
		}
		else if (revenue < 50)
		{
			if (country == "UAE" || country == "Turkey") 
			{
				licenseFee = 20 * BASE_RATE + ((revenue - 20) * SPECIAL_SECONDARY_RATE);
			}
			else
			{
				licenseFee = 20 * BASE_RATE + ((revenue - 20) * NORMAL_SECONDARY_RATE);
			}
		}
		else
		{
			if (country == "UAE" || country == "Turkey")
			{
				licenseFee = 20 * BASE_RATE + 30 * SPECIAL_SECONDARY_RATE + ((revenue - 50) * TERTIARY_RATE);
			}
			else
			{
				licenseFee = 20 * BASE_RATE + 30 * NORMAL_SECONDARY_RATE + ((revenue - 50) * TERTIARY_RATE);
			}
		}
		//prints out the result
		cout << "The license fee for " << company << " is $" << licenseFee << " million." << endl;
	}
}
