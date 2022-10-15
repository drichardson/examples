import UIKit

var greeting = "Hello, playground"

let d = 1.234
d


struct S
{
    let x : Int
    var y : Int
}

extension S {
    init() {
        x = 2
        y = 4
    }
}

S(x: 1, y: 2)

S(x: 1, y: 2)
S()


1 ?? 123 ?? nil

var x : Int?
var y : Int?

x ?? y ?? 1

enum Test {
    case one
    case two(i1 : Int)
}

let t = Test.one
print(t)

let t2 = Test.two(i1: 123)
print(t2)

enum Test2 {
    case clientSystemMeasurementEvent(
        //
        // OS/Application Metrics
        //
        systemThermalLevelIos: ProcessInfo.ThermalState,
        applicationStateIos: UIApplication.State,
        
        //
        // Real Time Communication (Audio/Video) Metrics
        //
        rtcProvider : String?,
        
        // RTC Transport
        rtcBytesSent : Int?,
        rtcBytesReceived : Int?,
        
        // Local Video Source Stats (Sender side)
        localVideoUserId : String?,
        localVideoCodec : String?,
        localVideoWidth : Int?,
        localVideoHeight : Int?,
        localVideoFrames : Int?,
        localVideoCapturedFramerate : Double?,
        localVideoRenderedFramerate : Double?,
        localVideoEncodedFramerate : Double?,
        localVideoTargetEncodedFramerate : Double?,
        localVideoSentFramerate : Double?,
        localVideoBitrate : Double?,
        localVideoTargetBitrate : Double?,
        localVideoPacketLoss : Double?,
        
        // Local Audio Source Stats (Sender side)
        localAudioUserId : String?,
        localAudioSampleRate : Int?,
        localAudioChannels : Int?,
        localAudioBitrate : Double?,
        localAudioPacketLoss : Double?,
        
        // Remote Seller Video Source Stats (Viewer side)
        sellerRemoteVideoUserId : String?,
        sellerRemoteVideoWidth : Int?,
        sellerRemoteVideoHeight : Int?,
        sellerRemoteVideoDecodedFramerate : Double?,
        sellerRemoteVideoRenderedFramerate : Double?,
        sellerRemoteVideoBitrate : Double?,
        sellerRemoteVideoPacketLoss : Double?,
        
        // Remote Seller Audio Source Stats (Viewer side)
        sellerRemoteAudioUserId : String?,
        sellerRemoteAudioSampleRate : Int?,
        sellerRemoteAudioChannels : Int?,
        sellerRemoteAudioBitrate : Double?,
        
        // Remote Co-Host Video Source Stats (Viewer side)
        cohostRemoteVideoUserId : String?,
        cohostRemoteVideoWidth : Int?,
        cohostRemoteVideoHeight : Int?,
        cohostRemoteVideoDecodedFramerate : Double?,
        cohostRemoteVideoRenderedFramerate : Double?,
        cohostRemoteVideoBitrate : Double?,
        cohostRemoteVideoPacketLoss : Double?,
        
        // Remote Co-Host Audio Source Stats (Viewer side)
        cohostRemoteAudioUserId : String?,
        cohostRemoteAudioSampleRate : Int?,
        cohostRemoteAudioChannels : Int?,
        cohostRemoteAudioBitrate : Double?
    )
}

let t2 = Test2.clientSystemMeasurementEvent(systemThermalLevelIos: .critical, applicationStateIos: .active, rtcProvider: nil, rtcBytesSent: nil, rtcBytesReceived: nil, localVideoUserId: nil, localVideoCodec: nil, localVideoWidth: nil, localVideoHeight: nil, localVideoFrames: nil, localVideoCapturedFramerate: nil, localVideoRenderedFramerate: nil, localVideoEncodedFramerate: nil, localVideoTargetEncodedFramerate: nil, localVideoSentFramerate: nil, localVideoBitrate: nil, localVideoTargetBitrate: nil, localVideoPacketLoss: nil, localAudioUserId: nil, localAudioSampleRate: nil, localAudioChannels: nil, localAudioBitrate: nil, localAudioPacketLoss: nil, sellerRemoteVideoUserId: nil, sellerRemoteVideoWidth: nil, sellerRemoteVideoHeight: nil, sellerRemoteVideoDecodedFramerate: nil, sellerRemoteVideoRenderedFramerate: nil, sellerRemoteVideoBitrate: nil, sellerRemoteVideoPacketLoss: nil, sellerRemoteAudioUserId: nil, sellerRemoteAudioSampleRate: nil, sellerRemoteAudioChannels: nil, sellerRemoteAudioBitrate: nil, cohostRemoteVideoUserId: nil, cohostRemoteVideoWidth: nil, cohostRemoteVideoHeight: nil, cohostRemoteVideoDecodedFramerate: nil, cohostRemoteVideoRenderedFramerate: nil, cohostRemoteVideoBitrate: nil, cohostRemoteVideoPacketLoss: nil, cohostRemoteAudioUserId: nil, cohostRemoteAudioSampleRate: nil, cohostRemoteAudioChannels: nil, cohostRemoteAudioBitrate: nil)

t2
