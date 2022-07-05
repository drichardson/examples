import socketserver

class MyHandler(socketserver.DatagramRequestHandler):
    def __init__(self, *args, **kwargs):
        socketserver.DatagramRequestHandler.__init__(self, *args, **kwargs)
        print("handler initialized")

    def handle(self):
        print("handle called")
        msg = self.rfile.read().decode('utf-8')
        print("Got message: ", msg)
        self.wfile.write(f"ACK len {len(msg)}".encode('utf-8'))

#server = socketserver.ThreadingUDPServer(("127.0.0.1", 8123), MyHandler)
server = socketserver.UDPServer(("127.0.0.1", 8123), MyHandler)
server.serve_forever()
