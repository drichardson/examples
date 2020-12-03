#include <iostream>
#include <string>
#include <vector>
#include <list>
#include <stack>
#include <queue>
#include <deque>
#include <set>
#include <map>
#include <iterator>
#include <algorithm>
#include <functional>

#include <stdlib.h>

#include <stdexcept>

using namespace std;

static void heading(const string & msg)
{
	string s(msg.length(), '-');
	cout << s << '\n' << msg << '\n' << s << endl;
}

// printElements only uses the increment operator in read only
// mode on the internal iterator to maximize the number of
// iterator types (i.e. forward only, forward only const, random access)
// that can use this template function.
template<class T> void printElements(T begin, T end)
{	
	bool pastFirst = false;
	for(T i = begin; i != end; ++i) {
		if(!pastFirst) {
			pastFirst = true;
		} else {
			cout << ",";
		}
		cout << *i;
	}
	cout << endl;
}

template<class T> void printAllElements(const T & container)
{
	printElements(container.begin(), container.end());
}

template<class T> void printAllElements(const string & msg, const T & container)
{
	cout << msg << ": ";
	printElements(container.begin(), container.end());
}

template<class T> void printMapElements(T begin, T end)
{	
	bool pastFirst = false;
	for(T i = begin; i != end; ++i) {
		if(!pastFirst) {
			pastFirst = true;
		} else {
			cout << ",";
		}
		cout << i->first << "->" << i->second;
	}
	cout << endl;
}

template<class T> void printAllMapElements(const T & container)
{
	printMapElements(container.begin(), container.end());
}

template<class T> void printAllMapElements(const string & msg, const T & container)
{
	cout << msg << ": ";
	printMapElements(container.begin(), container.end());
}

static void testString()
{
	heading("string");
	
	string s1 = "This is a test";
	string s2 = "This is also a test";
	string s3 = "This is a test";
	
	cout << "(s1 == s2) = " << (s1 == s2) << endl;
	cout << "(s1 == s3) = " << (s1 == s3) << endl;

	printAllElements(s1);
	printElements(s1.rbegin(), s1.rend());
	
	cout << "s1: '" << s1 << "' (cout)" << endl;
	printf("s1: '%s' (printf)\n", s1.c_str());
	
	s1.append(". An appended string.");
	cout << "s1: '" << s1 << "'" << endl;
	
	string toFind = "n appended";
	string replaceWith = " concatenated";
	cout << "find position: " << s1.find(toFind) << endl;
	s1.replace(s1.find(toFind), toFind.size(), replaceWith);
	cout << "s1: '" << s1 << "'" << endl;
	
	s1.clear();
	cout << "s1: '" << s1 << "'" << endl;
}

class RandomGeneratorModuloX
{
	const int mX;
public:
	RandomGeneratorModuloX(int x, long seed = 1) : mX(x) {
		if(x < 1)
			throw invalid_argument("Invalid X Argument to RandomGeneratorModuloX. Must be > 0");
		srandom(seed);
	}
	
	int operator() ()
	{
		return random() % mX;
	}
};

static void testVector()
{
	heading("vector");
	vector<int> v;

	v.insert(v.begin(), 1);
	v.insert(v.begin(), 2);
	v.insert(v.begin(), 3);
	v.insert(v.end(), 4);
	v.insert(v.end(), 5);
	v.insert(v.end(), 6);
	printAllElements(v);
	printElements(v.rbegin(), v.rend());
	
	v[0] = 100;
	printAllElements(v);
	v[v.size() - 1] = 101;
	printAllElements(v);
	
	v.push_back(1000);
	v.push_back(1001);
	printAllElements("Push 2 numbers", v);
	v.pop_back();
	printAllElements("Pop 1", v);
	v.pop_back();
	printAllElements("Pop 2", v);
	
	sort(v.begin(), v.end());
	printAllElements("Sorted (no comp argument)", v);
	
	sort(v.begin(), v.end(), greater<int>());
	printAllElements("Sorted (greater)", v);
	
	RandomGeneratorModuloX randomLessThan100(100);
	generate(v.begin(), v.end(), randomLessThan100);
	printAllElements("Replace with random numbers", v);
	
	sort(v.begin(), v.begin() + v.size() / 2);
	sort(v.begin() + (v.size() / 2) + 1, v.end());
	printAllElements("Partitioned sort", v);
	
	sort(v.begin(), v.end());
	printAllElements("All Sorted", v);
	
	v.clear();
	printAllElements(v);
}

static void testDeque()
{
	heading("deque");
	
	deque<int> q;
	deque<int>::const_iterator i;
	
	i = min_element(q.begin(), q.end());
	if(i == q.end())
		cout << "No elements - cannot compute minimum" << endl;
	else
		cout << "The minimum element is " << *i << endl;
	
	q.push_back(23);
	q.push_back(35);
	printAllElements("q", q);
	
	q.push_front(0);
	q.push_front(48);
	printAllElements("q", q);
	
	
	i = min_element(q.begin(), q.end());
	if(i == q.end())
		cout << "No elements - cannot compute minimum" << endl;
	else
		cout << "The minimum element is " << *i << endl;
	
	i = max_element(q.begin(), q.end());
	if(i == q.end())
		cout << "No elements - cannot compute maximum" << endl;
	else
		cout << "The maximum element is " << *i << endl;
	
	const deque<int>::size_type Q2_ELEMENT_COUNT = 5;
	deque<int> q2(Q2_ELEMENT_COUNT);
	RandomGeneratorModuloX gen(50);
	generate(q2.begin(), q2.end(), gen);
	printAllElements("q2", q2);
	
	// Turn q2 into a heap. A heap allows the elements to be removed from
	// largest to smallest with logarithmic complexity. The pop_heap operation
	// doesn't remove the element, it simply moves it to the end of
	// the containers. Therefore, the programmer must keep track of the number
	// of elements left in the heap - subtracting by 1 after each pop_heap.
	// pop_heap requires the range to be a valid heap. Use make heap to turn
	// a container with a random access iterator into a heap.
	// Note: is_heap is an extention, not part of the C++ standard.
	// Since exentions weren't enabled, it must be called with __is_heap.
	cout << "Is q2 a heap? " << __is_heap(q2.begin(), q2.end()) << endl;
	make_heap(q2.begin(), q2.end());
	cout << "Is q2 a heap? " << __is_heap(q2.begin(), q2.end()) << endl;
	cout << "Pop All Heap Elements" << endl;
	for(deque<int>::size_type curSize = 0; curSize < q2.size(); ++curSize) {
		cout << " " << *q2.begin() << endl;
		pop_heap(q2.begin(), q2.end() - curSize);
	}

	printAllElements(q2);	
}

static void testList()
{
	heading("list");
	
	list<double> l;
	
	l.push_back(3.14159265);
	l.push_back(2.71828183);
	printAllElements("push back", l);
	
	l.push_front(0);
	l.push_front(1);
	printAllElements("push front", l);
	
	l.sort();
	printAllElements("sorted", l);
	
	l.sort(greater<double>());
	printAllElements("sorted gt", l);
	
	l.reverse();
	printAllElements("reverse", l);
	
	list<double> l2;
	l2.push_back(2.9);
	l2.push_back(1.41421356);
	l2.push_back(2);
	printAllElements("l2", l2);
	
	// Both *this and x must be sorted according to operator<, and they must
	// be distinct. (That is, it is required that &x != this.) This function
	// removes all of x's elements and inserts them in order into *this. 
	l.sort();
	l2.sort();
	printAllElements("l sorted", l);
	printAllElements("l2 sorted", l2);
	l.merge(l2);
	printAllElements("l merged", l);
	printAllElements("l2 merged into l", l2);
}

static void testStack()
{
	heading("stack");
	
	// stack is a container adapter; it is implemented on top of another
	// container. By default, deque is the container, but others may be specified.
	stack<string> s;
	s.push("2");
	s.push("5");
	s.push("-");
	s.push("3");
	s.push("+");
	
	while(!s.empty()) {
		cout << "Top of Stack: " << s.top() << endl;
		s.pop();
	}
}

static void testQueue()
{
	heading("queue");
	
	queue<string> q;
	q.push("Steven");
	q.push("Hugo");
	q.push("Kim");
	q.push("Johnson");
	
	cout << "First person to enter the line was " << q.front() << endl;
	cout << "Last person to enter the line was " << q.back() << endl;
	
	while(!q.empty()) {
		cout << "Next in line is: " << q.front() << endl;
		q.pop();
	}
}

static void testSet()
{
	heading("set");
	
	int oddArray[] = { 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29 };
	int evenArray[] = { 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30 };
	
	set<int> odd(oddArray, oddArray + sizeof(oddArray)/sizeof(oddArray[0]));
	set<int> even(evenArray, evenArray + sizeof(evenArray)/sizeof(evenArray[0]));
	set<int> prime;
	prime.insert(1);
	for(int i = 2; i < 30; ++i) {
		bool isPrime = true;
		for(int divisor = 2; divisor < i; ++divisor) {
			if(i % divisor == 0) {
				isPrime = false;
				break;
			}
		}
		if(isPrime) prime.insert(i);
	}
	
	printAllElements("odd", odd);
	printAllElements("prime", prime);
	printAllElements("even", even);
	even.erase(30);
	
	cout << "even (using copy for output): ";
	copy(even.begin(), even.end(), ostream_iterator<int>(cout, ":"));
	cout << endl;
	
	set <int> natural;
	set_union(odd.begin(), odd.end(),
			  even.begin(), even.end(),
			  inserter(natural, natural.begin()));
	printAllElements("natural", natural);
	
	set <int> multiplesOf8;
	multiplesOf8.insert(8);
	multiplesOf8.insert(16);
	multiplesOf8.insert(24);
	multiplesOf8.insert(32);
	multiplesOf8.insert(40);
	printAllElements("multiplesOf8", multiplesOf8);
	
	set<int> result;
	set_intersection(even.begin(),even.end(),
					 multiplesOf8.begin(),multiplesOf8.end(),
					 inserter(result, result.begin()));
	printAllElements("even AND multiplesOf8", result);
	result.clear();
	printAllElements("Empty Check", result);
	
	set_difference(natural.begin(), natural.end(),
				   even.begin(), even.end(),
				   inserter(result, result.begin()));
	printAllElements("natural - even", result);
	
	result.clear();
	set_symmetric_difference(even.begin(), even.end(),
							 multiplesOf8.begin(), multiplesOf8.end(),
							 inserter(result, result.begin()));
	printAllElements("symmetric difference of even and multiplesOf8", result);
	
	cout << "natural includes even? " <<
		includes(natural.begin(), natural.end(),
				 even.begin(), even.end()) << endl;

	cout << "even includes natural? " <<
		includes(even.begin(), even.end(),
				 natural.begin(), natural.end()) << endl;
}

static void testMap()
{
	heading("map");
	
	map<string, unsigned> peopleToAge;
	
	peopleToAge["Rick"] = 46;
	peopleToAge["Beth"] = 12;
	peopleToAge["Martin"] = 89;
	peopleToAge["Alex"] = 25;
	
	peopleToAge.insert(make_pair("Howie", 33));
	peopleToAge.insert(make_pair("Ariel", 18));
	
	printAllMapElements(peopleToAge);
	
	map<string, unsigned>::iterator itr = peopleToAge.find("Martin");
	if(itr == peopleToAge.end())
		cout << "Martin not found" << endl;
	else
		cout << "Found Martin. He is " << itr->second << " years old." << endl;
	
	itr = peopleToAge.find("Randal");
	if(itr == peopleToAge.end())
		cout << "Randal not found" << endl;
	else
		cout << "Found Randal. He is " << itr->second << " years old." << endl;
}

static void testMultiMap()
{
	heading("multimap");
	
	multimap<int, string> ageToPeople;
	
	ageToPeople.insert(make_pair(13, "Laura"));
	ageToPeople.insert(make_pair(18, "Mandel"));
	ageToPeople.insert(make_pair(52, "Harvey"));
	ageToPeople.insert(make_pair(35, "Manuel"));
	ageToPeople.insert(make_pair(18, "Tyrone"));
	ageToPeople.insert(make_pair(35, "Winston"));
	
	printAllMapElements(ageToPeople);
	
	cout << "Number of people who are 35: " << ageToPeople.count(35) << endl;
	cout << "Number of people who are 36: " << ageToPeople.count(36) << endl;
	
	cout << "People who are 35:" << endl;
	pair<multimap<int, string>::iterator, multimap<int, string>::iterator> range =
		ageToPeople.equal_range(35);
	for(multimap<int, string>::iterator i = range.first; i != range.second; ++i) {
		cout << " " << i->second << endl;
	}
}

int main (int argc, char * const argv[]) {

	cout << "C++ Review using the STL (Standard Template Library)" << endl;
	
	testString();
	testVector();
	testDeque();
	testList();
	testStack();
	testQueue();
	testSet();
	testMap();
	testMultiMap();
	
    return 0;
}
