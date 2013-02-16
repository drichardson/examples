//
//  SPProxyServer.m
//  SimpleProxy
//
//  Created by Douglas Richardson on 2/11/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import "SPProxyServer.h"
#import "SPLog.h"
#import "SPClientRecord.h"
#import <netinet/in.h>
#import <netdb.h>
#import <arpa/inet.h>
#import <sys/socket.h>
#import <sys/types.h>
#import <sys/event.h>
#import <sys/time.h>

@implementation SPProxyServer
{
    BOOL _keepRunning;
}

@synthesize delegate;

- (void)start
{
    assert(!_keepRunning);
    
    if ( _keepRunning )
    {
        return;
    }
    
    _keepRunning = YES;
    [NSThread detachNewThreadSelector:@selector(_serverThread:) toTarget:self withObject:nil];
}

- (void)stop
{
    _keepRunning = NO;
}

static void CloseRecord(NSMutableDictionary* records, SPClientRecord* record)
{    
    [record retain]; // Retain since the only thing retraining may be the dictionary.
    
    if ( record.fdClient )
    {
        [records removeObjectForKey:[NSNumber numberWithInt:record.fdClient]];
    }
    
    if ( record.fdServer )
    {
        [records removeObjectForKey:[NSNumber numberWithInt:record.fdServer]];
    }
    
    [record closeSockets];
    [record release];
}

static BOOL SendData(int fd, NSData* data)
{
    const char* p = [data bytes];
    int bytesSent = 0;
    int bytesToSend = (int)[data length];
    while( bytesSent < bytesToSend )
    {
        ssize_t rc = send(fd, p + bytesSent, bytesToSend - bytesSent, 0);
        if ( rc == -1 )
        {
            SPLogClass(LOG_DEBUG, [SPProxyServer class], @"Error sending data %d: %s", errno, strerror(errno));
            break;
        }
        else
        {
            bytesSent += rc;
        }
    }
    
    return bytesSent == [data length] ? YES : NO;
}

- (void)_serverThread:(id)arg
{
    int listeningSocket = socket(PF_INET, SOCK_STREAM, 0);
    if ( listeningSocket == -1 )
    {
        [self.delegate proxyServer:self fatalErrorDidOccur:[NSError errorWithDomain:NSPOSIXErrorDomain code:errno userInfo:nil]];
        _keepRunning = NO;
        return;
    }
    
    
    int enableAddressReuse = 1;
    int rc = setsockopt(listeningSocket, SOL_SOCKET, SO_REUSEADDR, &enableAddressReuse, sizeof(enableAddressReuse));
    if ( rc == -1 )
    {
        // Unexpected but non-fatal error. Log a message and continue.
        SPLogClass(LOG_WARNING, self, @"Error setting reuse address flag on listening socket. %d: %s", errno, strerror(errno));
    }
    
    
    struct sockaddr_in httpAddr;
    httpAddr.sin_family = AF_INET;
    httpAddr.sin_len = sizeof(httpAddr);
    httpAddr.sin_port = htons(8888);
    httpAddr.sin_addr.s_addr = INADDR_ANY;
    
    rc = bind(listeningSocket, (struct sockaddr*)&httpAddr, sizeof(httpAddr));
    if ( rc != 0 )
    {        
        [self.delegate proxyServer:self fatalErrorDidOccur:[NSError errorWithDomain:NSPOSIXErrorDomain code:errno userInfo:nil]];
        _keepRunning = NO;
        close(listeningSocket);
        return;
    }
    
    
    rc = listen(listeningSocket, 5);
    if ( rc != 0 )
    {
        [self.delegate proxyServer:self fatalErrorDidOccur:[NSError errorWithDomain:NSPOSIXErrorDomain code:errno userInfo:nil]];
        _keepRunning = NO;
        close(listeningSocket);
        return;
    }
    
    // TODO: IPv6 socket too.
    
    // Setup kqueue
    
    int kq = kqueue();
    if ( kq == -1 )
    {
        [self.delegate proxyServer:self fatalErrorDidOccur:[NSError errorWithDomain:NSPOSIXErrorDomain code:errno userInfo:nil]];
        _keepRunning = NO;
        close(listeningSocket);
        return;
    }
    
    // Add listening socket read filter (let's me know when a new connection is made)
    struct kevent change;
    EV_SET(&change, listeningSocket, EVFILT_READ, EV_ADD | EV_ENABLE, 0, 0, 0);
    rc = kevent(kq, &change, 1, NULL, 0, NULL);
    if ( rc == -1 )
    {
        [self.delegate proxyServer:self fatalErrorDidOccur:[NSError errorWithDomain:NSPOSIXErrorDomain code:errno userInfo:nil]];
        _keepRunning = NO;
        close(listeningSocket);
        close(kq);
        return;
    }
    
    SPLogClass(LOG_DEBUG, self, @"Waiting for connection");
    
    // Dictionary of SPClientRecords keyed by file descriptors (client and server side). There can
    // be up to 2 entries for a single client: the client to proxy socket and the proxy to server socket.
    NSMutableDictionary* clientRecords = [NSMutableDictionary dictionary];
    
    while (_keepRunning)
    {
        @autoreleasepool {
            
            struct kevent event;
            int eventCount = kevent(kq, NULL, 0, &event, 1, NULL);
            
            if ( eventCount == -1 )
            {
                SPLogClass(LOG_ERR, self, @"Main kevent listener failed. %d: %s", errno, strerror(errno));
                // TODO: Determine whether I should try again or fail.
                continue;
            }
            
            SPLogClass(LOG_DEBUG, self, @"Got %d events", eventCount);
            
            if ( event.ident == listeningSocket )
            {
                SPLogClass(LOG_DEBUG, self, @"Got a connection on the listening socket.");
                int fd = accept(listeningSocket, NULL, NULL);
                if ( fd == -1 )
                {
                    SPLogClass(LOG_ERR, self, @"Error accepting connection. %d: %s", errno, strerror(errno));
                }
                else
                {
                    struct kevent change;
                    EV_SET(&change, fd, EVFILT_READ, EV_ADD | EV_ENABLE | EV_EOF, 0, 0, 0);
                    int rc = kevent(kq, &change, 1, NULL, 0, NULL);
                    if ( rc != -1 )
                    {
                        // Create an identifer that will be used to identify this client on subsequent delegate calls
                        SPClientRecord* record = [SPClientRecord new];
                        record.fdClient = fd;
                        [clientRecords setObject:record forKey:[NSNumber numberWithInt:fd]];
                        [record release];
                    }
                    else
                    {
                        SPLogClass(LOG_ERR, self, @"Couldn't add accepted connection to kqueue. %d: %s", errno, strerror(errno));
                        close(fd);
                    }
                }
            }
            else
            {
                int fdEvent = (int)event.ident;
                SPClientRecord* record = [clientRecords objectForKey:[NSNumber numberWithInt:fdEvent]];
                SPLogClass(LOG_DEBUG, self, @"%d bytes available from client %@ (fd: %d)", event.data, record, fdEvent);
                assert(record);
                assert(event.data >= 0);
                
                if ( event.data > 0 )
                {
                    NSMutableData* data = [NSMutableData dataWithLength:event.data];
                    ssize_t bytesRead = read(fdEvent, [data mutableBytes], [data length]);
                    if ( bytesRead == -1 )
                    {
                        SPLogClass(LOG_ERR, self, @"Error reading bytes from client %@ (fd: %d). %d: %s", record, fdEvent, errno, strerror(errno));
                    }
                    else if ( bytesRead >= 0 )
                    {                    
                        assert(bytesRead >= 0 && bytesRead <= [data length]);
                        [data setLength:bytesRead];
                        
                        if ( fdEvent == record.fdClient )
                        {
                            [self.delegate proxyServer:self didReceiveRequest:data forConnection:record.identifier];
                            
                            // Parse the request and re-send it to the server.
                            [record addRequestData:data];
                            
                            if ( record.fdServer > 0 )
                            {
                                // We have a connection to the server. Simply forward the from the client to the server.
                                
                            }
                            else if ( record.host )
                            {
                                // Haven't made a connection to the server yet, but we do have the host information
                                // so we should be able to connect to the server now.
                                
                                NSString* hostLine = record.host;
                                
                                // If the host is set, we have enough information to contact the server on behalf
                                // of the proxy client.
                                NSString* host = nil;
                                NSInteger port = 80;
                                // TODO: Handle IPv6 addresses
                                NSArray* hostComponents = [hostLine componentsSeparatedByString:@":"];
                                if ( [hostComponents count] > 0 )
                                {
                                    host = [hostComponents objectAtIndex:0];
                                    
                                    if ( [hostComponents count] > 1 )
                                    {
                                        NSString* portStr = [hostComponents objectAtIndex:1];
                                        port = [portStr integerValue];
                                    }
                                    
                                    // TODO: First try to convert the hostname directly to an IP address.
                                    
                                    // TODO: Make this asynchronous and use functions that tell you what kind of address it resolves to instead of guessing (AF_INET) ahead of time.
                                    struct hostent* h = gethostbyname2([host UTF8String], AF_INET);
                                    if ( h && h->h_length > 0 )
                                    {
                                        assert(h->h_addrtype == AF_INET);
                                        
                                        struct in_addr** addrList = (struct in_addr**)h->h_addr_list;
                                        struct sockaddr_in saddr4;
                                        bzero(&saddr4, sizeof(saddr4));
                                        saddr4.sin_family = AF_INET;
                                        saddr4.sin_len = sizeof(saddr4);
                                        saddr4.sin_port = htons((short)port);
                                        saddr4.sin_addr = *addrList[0];
                                        
                                        record.fdServer = socket(PF_INET, SOCK_STREAM, 0);
                                        if ( record.fdServer != -1 )
                                        {                                    
                                            // TODO: Do this in a non-blocking fashion.
                                            int rc = connect(record.fdServer, (struct sockaddr*)&saddr4, sizeof(saddr4));
                                            if ( rc != -1 )
                                            {
                                                SPLogClass(LOG_DEBUG, self, @"Connected to server %@", host);
                                                
                                                // Send all the data I have.
                                                NSData* requestData = record.request;
                                                // TODO: Use MSG_NOSIGNAL, MSG_DONTWAIT? If you have data to send still,
                                                // put it in the kqueue until all the data is sent and then remove the
                                                // descriptor for the kqueue.
                                                BOOL rc = SendData(record.fdServer, requestData);
                                                if ( rc )
                                                {
                                                    struct kevent change;
                                                    EV_SET(&change, record.fdServer, EVFILT_READ, EV_ADD | EV_ENABLE | EV_EOF, 0, 0, 0);
                                                    int rc = kevent(kq, &change, 1, NULL, 0, NULL);
                                                    if ( rc != -1 )
                                                    {
                                                        // TODO: Should get removed when server socket closed. Look out for race conditions.
                                                        [clientRecords setObject:record forKey:[NSNumber numberWithInt:record.fdServer]];
                                                    }
                                                    else
                                                    {
                                                        SPLogClass(LOG_ERR, self, @"Couldn't add server connection file descriptor to kqueue. %d: %s", errno, strerror(errno));
                                                        CloseRecord(clientRecords, record);
                                                    }
                                                }
                                                else
                                                {
                                                    SPLogClass(LOG_ERR, self, @"Failed to send request data to server.");
                                                    CloseRecord(clientRecords, record);
                                                }
                                            }
                                            else
                                            {
                                                SPLogClass(LOG_WARNING, self, @"Error connecting to server. %d: %s", errno, strerror(errno));
                                                CloseRecord(clientRecords, record);
                                            }
                                        }
                                        else
                                        {
                                            SPLogClass(LOG_ERR, self, @"Error creating socket to connect to host %@", host);
                                            // TODO: callback delegate here
                                        }
                                    }
                                    else
                                    {
                                        SPLogClass(LOG_DEBUG, self, @"Couldn't resolve hostname %@", host);
                                        // TODO: Callback the delegate here.
                                    }
                                }
                            }
                        }
                        else if ( fdEvent == record.fdServer )
                        {
                            [self.delegate proxyServer:self didReceiveResponse:data forConnection:record.identifier];
                            
                            // Send the response to the client.
                            BOOL rc = SendData(record.fdClient, data);
                            if ( !rc )
                            {
                                SPLogClass(LOG_ERR, self, @"Error sending response to client.");
                            }
                        }
                        else
                        {
                            assert(0);
                        }
                        
                    }
                }
                else if ( event.flags & EV_EOF )
                {
                    SPLogClass(LOG_DEBUG, self, @"EOF for client record %@", record.identifier);
                    CloseRecord(clientRecords, record);
                }
                else
                {
                    // wtf
                    assert(0);
                    close(fdEvent);
                }
            }
            
        }
    }
    
    close(listeningSocket);
    close(kq);
    
    SPLogClass(LOG_DEBUG, self, @"Server thread ended");
}

@end
