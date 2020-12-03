//
//  SPClientRecord.m
//  SimpleProxy
//
//  Created by Douglas Richardson on 2/14/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import "SPClientRecord.h"
#import "NSString+SPAdditions.h"

@interface SPClientRecord ()
- (void)_closeServerSocket;
- (void)_closeClientSocket;
@end

@implementation SPClientRecord
{
    NSMutableData* _request;
    NSMutableData* _response;
    NSInteger _currentLocation;
}

@synthesize identifier=_identifier, fdServer=_fdServer, fdClient=_fdClient, host=_host, request=_request;

- (id)init
{
    self = [super init];
    if ( self )
    {
        _identifier = [[NSString uuid] retain];
        _request = [NSMutableData new];
        _response = [NSMutableData new];
        _currentLocation = 0;
        _fdServer = -1;
        _fdClient = -1;
    }
    return self;
}

- (void)dealloc
{
    [_identifier release];
    [_request release];
    [_response release];
    [_host release];
    [self closeSockets];
    [super dealloc];
}

- (NSString*)description
{
    return [NSString stringWithFormat:@"%@ %@", super.description, self.identifier];
}

- (void)addRequestData:(NSData*)data
{
    [_request appendData:data];
    
    if ( _host )
    {
        // Got what I need.
        return;
    }
    
    const char* CRLF = "\r\n";
    // TODO: Stop reading if you hit the end of the buffer. Test by feeding 1 byte at a time.
    
    while (true)
    {
        const char* startLine = [_request bytes] + _currentLocation;    
        const char* endLine = strstr(startLine, CRLF);
        
        if ( endLine != NULL )
        {
            // Got a line.
            const NSInteger lineLen = endLine - startLine;
            
            if ( _host == nil && strncmp("Host:", startLine, 5) == 0 )
            {
                const char* hostStart = startLine + 5; // skip past 'Host:'
                const NSInteger hostLen = endLine - hostStart;
                NSString* host = [[NSString alloc] initWithBytes:hostStart length:hostLen encoding:NSASCIIStringEncoding];
                _host = [[host stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]] retain];
                [host release];
                
                break; // all done
            }
            
            _currentLocation = lineLen + 2; // Skip past the CRLF.
        }
        else
        {
            break;
        }
    }
}

- (NSData*)request
{
    return _request;
}

- (void)setFdServer:(int)fdServer
{
    if ( fdServer != _fdServer )
    {
        [self _closeServerSocket];
        _fdServer = fdServer;
    }
}

- (void)_closeServerSocket
{
    if ( _fdServer > 0 )
    {
        close(_fdServer);
        _fdServer = -1;
    }
}

- (void)setFdClient:(int)fdClient
{
    if ( fdClient != _fdClient )
    {
        [self _closeClientSocket];
        _fdClient = fdClient;
    }
}

- (void)_closeClientSocket
{
    if ( _fdClient > 0 )
    {
        close(_fdClient);
        _fdClient = -1;
    }
}


- (void)closeSockets
{
    [self _closeServerSocket];
    [self _closeClientSocket];
}

@end
