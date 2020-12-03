//
//  main.m
//  FastCGITest
//
//  Created by Douglas Richardson on 3/30/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <fcgiapp.h>

int main(int argc, const char * argv[])
{

    @autoreleasepool {
        
        if ( FCGX_Init() != 0 )
        {
            NSLog(@"FCGX_Init failed.");
            exit(1);
        }
        
        int socket = FCGX_OpenSocket("/tmp/FastCGITest_socket", 5);
        if ( socket == -1 )
        {
            NSLog(@"FCGX_OpenSocket failed");
            exit(1);
        }
        
        FCGX_Request request;
        if ( FCGX_InitRequest(&request, socket, 0) != 0 )
        {
            NSLog(@"FCGX_InitRequest failed");
            exit(1);
        }
        
        while (1)
        {
            NSLog(@"Waiting for connection...");
            
            if ( FCGX_Accept_r(&request) != 0 )
            {
                NSLog(@"FCGX_Accept_r failed");
                exit(1);
            }
            
            NSLog(@"Accepted connection. Sending output.");
            
            FCGX_PutS("HTTP/1.1 200 OK\r\n", request.out);
            FCGX_PutS("Content-Type: text/plain\r\n", request.out);
            FCGX_PutS("\r\n", request.out);
            FCGX_PutS("Hello, world!", request.out);
        }
    }
    return 0;
}

