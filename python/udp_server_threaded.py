import socket
import socketserver
import time

class MyHandler(socketserver.DatagramRequestHandler):
    def __init__(self, *args, **kwargs):
        socketserver.DatagramRequestHandler.__init__(self, *args, **kwargs)
        print("handler initialized")

    def handle(self):
        print("handle called")
        msg = self.rfile.read().decode('utf-8')
        print("Got message: ", msg)
        self.wfile.write(f"ACK len {len(msg)}".encode('utf-8'))

server = socketserver.ThreadingUDPServer(("127.0.0.1", 8123), MyHandler)
#server.serve_forever()

with server:

    sock = socket.socket(family=socket.AF_INET, type=socket.SOCK_DGRAM)

    dest = ("127.0.0.1", 8123)

    sock.sendto(f"Test Message sent at {time.time()}".encode('utf-8'), dest)
    (msg, srcaddr) = sock.recvfrom(1024)
    msg = msg.decode("utf-8")

    print(f"Reply from {srcaddr}: msg: {msg}")

