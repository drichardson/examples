//
//  main.m
//  FromScratch
//
//  Created by Doug on 3/11/08.
//  Copyright __MyCompanyName__ 2008. All rights reserved.
//

#import <UIKit/UIKit.h>

int main(int argc, char *argv[])
{
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    int retVal = UIApplicationMain(argc, argv, nil, @"FromScratchAppDelegate");
    [pool release];
    return retVal;
}
