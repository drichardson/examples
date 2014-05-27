CXXFLAGS=-std=c++1y
TARGETS=thread_id

thread_id: thread_id.o
	g++ -pthread $< -o $@ 

clean:
	$(RM) $(TARGETS) *.o
