# get g++ 4.8.2 from brew so we get more C++ 11 support. Xcode's
# current clang kind of sucks.
CXX=/usr/local/bin/g++-4.8

lispy: parser.cpp
	$(CXX) -std=c++11 -g -o lispy parser.cpp

clean:
	$(RM) -r lispy lispy.dSYM
