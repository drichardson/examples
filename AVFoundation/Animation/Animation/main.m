//
//  main.m
//  Animation
//
//  Created by Douglas Richardson on 5/4/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <AVFoundation/AVFoundation.h>

static BOOL NewPixelBufferAndContextForFrameSize(CVPixelBufferRef* pixelBuffer, CGContextRef* pContext, CGSize frameSize)
{
    NSDictionary *options = [NSDictionary dictionaryWithObjectsAndKeys:
                             [NSNumber numberWithBool:YES], kCVPixelBufferCGImageCompatibilityKey,
                             [NSNumber numberWithBool:YES], kCVPixelBufferCGBitmapContextCompatibilityKey,
                             nil];
    CVPixelBufferRef pxbuffer = NULL;
    CVReturn status = CVPixelBufferCreate(kCFAllocatorDefault, frameSize.width, frameSize.height, kCVPixelFormatType_32ARGB, (__bridge CFDictionaryRef)options, &pxbuffer);
    assert(status == kCVReturnSuccess && pxbuffer != NULL);
    
    CVPixelBufferLockBaseAddress(pxbuffer, 0);
    void *pxdata = CVPixelBufferGetBaseAddress(pxbuffer);
    assert(pxdata != NULL);
    
    CGColorSpaceRef rgbColorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef context = CGBitmapContextCreate(pxdata, frameSize.width,
                                                 frameSize.height, 8, 4*frameSize.width, rgbColorSpace, 
                                                 kCGImageAlphaNoneSkipFirst);
    assert(context);
    CGColorSpaceRelease(rgbColorSpace);
    
    CVPixelBufferUnlockBaseAddress(pxbuffer, 0);
    
    *pixelBuffer = pxbuffer;
    *pContext = context;
    
    return YES;
}

int main(int argc, const char * argv[])
{
    if ( argc != 2 )
    {
        fprintf(stderr, "Missing output filename.\nUsage: Animation <output_file>\n");
        exit(1);
    }

    @autoreleasepool {
        
        NSString* outputPath = [NSString stringWithUTF8String:argv[1]];
        
        NSError* error = nil;
        AVAssetWriter* assetWriter = [[AVAssetWriter alloc] initWithURL:[NSURL fileURLWithPath:outputPath] fileType:AVFileTypeQuickTimeMovie error:&error];
        
        if ( assetWriter == nil )
        {
            NSLog(@"Couldn't create asset writer. %@", error);
            exit(1);
        }
        
        const CGSize kFrameSize = CGSizeMake(640, 480);
        
        NSDictionary *videoSettings = [NSDictionary dictionaryWithObjectsAndKeys:
                                       AVVideoCodecH264, AVVideoCodecKey,
                                       [NSNumber numberWithInt:kFrameSize.width], AVVideoWidthKey,
                                       [NSNumber numberWithInt:kFrameSize.height], AVVideoHeightKey,
                                       nil];
        
        AVAssetWriterInput* writerInput = [AVAssetWriterInput assetWriterInputWithMediaType:AVMediaTypeVideo outputSettings:videoSettings];
        
        if ( writerInput == nil )
        {
            NSLog(@"Couldn't create writer input.");
            exit(1);
        }
        
        if ( ![assetWriter canAddInput:writerInput] )
        {
            NSLog(@"Can't add input for asset writer.");
            exit(1);
        }
        
        [assetWriter addInput:writerInput];
        
        [assetWriter startWriting];
        [assetWriter startSessionAtSourceTime:CMTimeMake(0, 30)];
        
        dispatch_queue_t writerQueue = dispatch_queue_create("writerqueue", 0);
        __block int frameNumber = 0;
        
        [writerInput requestMediaDataWhenReadyOnQueue:writerQueue usingBlock:^{
           
            for (; frameNumber < 60 && [writerInput isReadyForMoreMediaData]; frameNumber++ )
            {
                CGContextRef context = NULL;
                CVPixelBufferRef pixelBuffer = NULL;
                if ( !NewPixelBufferAndContextForFrameSize(&pixelBuffer, &context, kFrameSize) )
                {
                    NSLog(@"Error creating pixel buffer.");
                    exit(1);
                }
                
                CGContextSetRGBFillColor(context, ((float)frameNumber) / 60.0, 0, 0, 1);
                CGContextFillRect(context, CGRectMake(0, 0, kFrameSize.width, kFrameSize.height));
                
                CGContextSetRGBStrokeColor(context, 1, 1, 1, 1);
                CGContextSetLineWidth(context, 2);
                CGContextMoveToPoint(context, 0, 0);
                CGContextAddLineToPoint(context, frameNumber, frameNumber);
                CGContextStrokePath(context);
                CGContextFlush(context);
                
                CMVideoFormatDescriptionRef formatDescription = NULL;
                CMVideoFormatDescriptionCreateForImageBuffer(NULL, pixelBuffer, &formatDescription);
                CMSampleTimingInfo sampleTiming = kCMTimingInfoInvalid;
                sampleTiming.duration = CMTimeMake(1, 30);
                sampleTiming.decodeTimeStamp = kCMTimeInvalid;
                sampleTiming.presentationTimeStamp = CMTimeMake(frameNumber, 30);
                
                CMSampleBufferRef sampleBuffer = NULL;
                CMSampleBufferCreateForImageBuffer(NULL, pixelBuffer, true, NULL, NULL, formatDescription, &sampleTiming, &sampleBuffer);
                
                CFRelease(formatDescription);
                
                [writerInput appendSampleBuffer:sampleBuffer];
                
                CFRelease(sampleBuffer);
                CGContextRelease(context);
                CVPixelBufferRelease(pixelBuffer);
            }
            
            if ( frameNumber >= 60 )
            {
                [writerInput markAsFinished];
                
                if ( ![assetWriter finishWriting] )
                {
                    NSLog(@"Error writing file to disk");
                    exit(1);
                }
                
                exit(0);
            }
            
        }];
        
        dispatch_main();
    }
    return 0;
}

