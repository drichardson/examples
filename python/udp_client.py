import socket
import time

sock = socket.socket(family=socket.AF_INET, type=socket.SOCK_DGRAM)

dest = ("127.0.0.1", 8123)

sock.sendto(f"Test Message sent at {time.time()}".encode('utf-8'), dest)
(msg, srcaddr) = sock.recvfrom(1024)
msg = msg.decode("utf-8")

print(f"Reply from {srcaddr}: msg: {msg}")

