# ClientFactory and Protocol handle all the ptential events that arise when you start
# working with connections: when a connection is established, when a connnection
# attempt fails, when an existing connection is broken, or when data is
# received from the other side.

from twisted.internet import reactor, protocol

class QuickDisconnectProtocol(protocol.Protocol):
	def connectionMade(self):
		print "Connected to %s." % self.transport.getPeer().host
		self.transport.loseConnection()

class BasicClientFactory(protocol.ClientFactory):
	protocol = QuickDisconnectProtocol
	
	def clientConnectionLost(self, connector, reason):
		print "Lost connection: %s" % reason.getErrorMessage()
		reactor.stop()
		
	def clientConnectionFailed(self, connector, reason):
		print "Connection failed: %s" % reason.getErrorMessage()
		reactor.stop()

reactor.connectTCP('www.google.com', 80, BasicClientFactory())
reactor.run()
