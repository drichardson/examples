# VNCClient TODO List

## Features
1. ~~Draw screen with incremental updates.~~  Added 3/29/2007
2. ~~Pointer events.~~  Added 3/29/2007
3. ~~Key events.~~  Added 3/30/2007
4. ~~Help~~ . Added 3/29/2007
5. ~~Implement Bell message.~~  Added 3/29/2007
6. ~~Connect to dialog.~~  Added 3/30/2007
7. ~~Server cut message.~~  Added 3/30/2007
8. ~~Client paste message.~~  Added 3/31/2007
9. VNC Authentication
10. ~~Automatically set server native resolution.~~  Added 3/30/2007
11. Auto size the window if the server resolution is smaller than the client window. If the server resolution is too big, use scroll bars.
12. CopyRect encoding
13. Multiple connections.
14. ~~Hide and Re-open windows.~~  Added 3/30/2007
15. ~~Set the title of the window to the VNC server name.~~  Added 3/30/2007
16. Add a recent hosts menu.

## Bugs

1. ~~Network events stop UI from responding quickly and vice versa.~~ FIXED 3/29/2007 - Implemented non-blocking sends.
2. ~~Initial connection to RFB server is slow when using IP addresses. Could be a DNS issue. After I added a line to the /etc/hosts file the hosts file the problem went away.~~ FIXED 3/31/2007
3. ~~Connecting with hostname rather than IP address fails.~~  FIXED 3/31/2007
4. ~~A Copy (as in Copy/Paste) on the server causes the client to hang, probably because the client isn't handling the copy message.~~  FIXED 3/30/2007
5. Changing the resolution on the server causes the client to freeze.
6. ~~Connecting to an invalid host by IP address (such as 192.168.3.23 on my network) doesn't display a connection error.~~ FIXED 3/31/2007
7. ~~Disconnect only closes the VNC View Window - the connection is still opened. Need to call close on the streams.~~ FIXED 3/30/2007
8. The client doesn't work with RFB Protocol version 3.3. Version 3.7 is not tested.
9. Control + other keys doesn't work. For instance, when connected to a Windows PC, Control+C doesn't copy.
10. ~~Copy/Paste doesn't work in Address text field in connect to window.~~  FIXED 3/30/2007
11. ~~Port field not being used. 5900 is used every time.~~  FIXED 3/30/2007

