#include <iostream>
using namespace std;
#define MAX_SIZE 100
// ==================================================================
int CheckSumPossibility(int num, int arr[], int size);
// ==================================================================
int main()
{
	int arraySize;
	int arr[MAX_SIZE];
	int num;
	int returnVal;

	cin >> arraySize;
	cin >> num;

	for (int i = 0; i < arraySize; ++i)
	{
		cin >> arr[i];
	}

	returnVal = CheckSumPossibility(num, arr, arraySize);

	if (returnVal == 1)
	{
		cout << "Possible!" << endl;
	}

	else
	{
		cout << "Not possible!" << endl;
	}

	return 0;
}

// ==================================================================
int CheckSumPossibility(int num, int arr[], int size) {

	if (num == 0)
		return 1;

	if (size == 0 && num != 0)
		return 0;

	if (num < arr[size - 1])
		return CheckSumPossibility(num, arr, size - 1);  // REJECTED == do not include the current element

	int REJECTED = CheckSumPossibility(num, arr, size - 1);  // REJECTED == do not include the current element
	int SELECTED = CheckSumPossibility(num - arr[size - 1], arr, size - 1) ;  // SELECTED == include the current element

	return REJECTED || SELECTED;


	/* SELECTED: when including current element new num = num-curr */
	/*
		    CURR
		    / \
		   S   R
	*/

}
