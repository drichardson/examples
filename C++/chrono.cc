#include <iostream>
#include <chrono>

using std::cout;
using std::endl;
using std::chrono::seconds;
using std::chrono::milliseconds;
using namespace std::literals::chrono_literals;

int main()
{
    cout << "Sec: " << seconds{1}.count() << endl;
    cout << "Sec: " << seconds{2}.count() << endl;
    cout << "Millis: " << milliseconds{1}.count() << endl;
    cout << "Sec+Millis: " << (seconds{1} + milliseconds{1}).count() << endl;
    cout << "Sec+Millis: " << (seconds{1} + milliseconds{1}).count() << endl;
    // use operator""h and friends
    cout << "Weird: " << (1s + 1ms + 1ns).count() << endl;

    std::chrono::duration<float, std::milli> millis{102.352};
    cout << "Float: " << millis.count() << endl;
    cout << "Float+Nonfloat: " << (millis + (1s)).count() << endl;
}
