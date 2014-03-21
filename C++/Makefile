TARGETS=ofstream_test glog

all: $(TARGETS)

ofstream_test: ofstream_test.cc
	$(CXX) -g -O0 -o ofstream_test ofstream_test.cc

glog: glog.cc
	$(CXX) -g -O0 -o glog glog.cc -lglog

clean:
	$(RM) -r $(TARGETS) *.dSYM a.txt
