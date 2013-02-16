from time import sleep
from thread import start_new_thread

def _1HzThread(p1, p2):
	while 1:
		print "1Hz: p1=" + str(p1) + ", p2=" + p2
		sleep(1)
		
def _2HzThread():
	while 1:
		print "2Hz"
		sleep(0.5)
		
def _10HzThread():
	while 1:
		print "10Hz"
		sleep(0.1)
		

print "Starting threads"
start_new_thread(_1HzThread, ("Arg 1", "Arg 2"))
start_new_thread(_2HzThread, ())
start_new_thread(_10HzThread, ())

print "Sleeping"

while 1:
	sleep(100)
	
print "Ending"
		

