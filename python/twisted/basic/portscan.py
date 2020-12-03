from twisted.internet import reactor, defer
from connectiontest import testConnect

def handleAllResults(results, ports):
	for port, resultInfo in zip(ports, results):
		success, result = resultInfo
		if success:
			print "Connected to port %i" % port
	reactor.stop()
		
import sys
host = sys.argv[1]
ports = range(1, 201)
testers = [testConnect(host, port) for port in ports]
defer.DeferredList(testers, consumeErrors=True).addCallback(handleAllResults, ports)
reactor.run()
