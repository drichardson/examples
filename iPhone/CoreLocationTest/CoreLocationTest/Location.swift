//
//  Location.swift
//  CoreLocationTest
//
//  Created by Douglas Richardson on 2/18/15.
//  Copyright (c) 2015 True Labs, Inc. All rights reserved.
//

import Foundation
import CoreLocation
import UIKit

/*
func authStatusToString(status : CLAuthorizationStatus) -> String {
    switch status {
    case .NotDetermined:
        return "Not Determined"
    case .Restricted:
        return "Restricted"
    case .Denied:
        return "Denied"
    case .Authorized:
        return "Authorized"
    case .AuthorizedWhenInUse:
        return "AuthorizedWhenInUse"
    }
}
*/

extension CLAuthorizationStatus {
    func toString() -> String {
        switch self {
        case .NotDetermined:
            return "Not Determined"
        case .Restricted:
            return "Restricted"
        case .Denied:
            return "Denied"
        case .Authorized:
            return "Authorized"
        case .AuthorizedWhenInUse:
            return "AuthorizedWhenInUse"
        }
    }
}

@objc class Location : NSObject, CLLocationManagerDelegate {
    
    let manager : CLLocationManager
    
    override init() {
        manager = CLLocationManager()
        super.init()
        manager.delegate = self
    }
    
    deinit {
        println("Location deinit")
        manager.delegate = nil
    }
    
    func locationManager(manager: CLLocationManager!, didChangeAuthorizationStatus status: CLAuthorizationStatus) {
        println("Authorization status changed to \(status.toString())")
        
        switch status {
        case .NotDetermined:
            println("Authorization Status is Not Determined. Asking user for always authorization")
            // Asking for always authorization requires the following Info.plist key: NSLocationAlwaysUsageDescription
            manager.requestAlwaysAuthorization()
        case .Authorized:
            //println("Yay! Authorized. Calling startUpdatingLocation")
            //manager.startUpdatingLocation()
            
            if let regions = manager.monitoredRegions {
                println("Monitoring \(regions.count) regions")
                for r in regions {
                    println("  region: \(r)")
                }
            }
            
            println("Start monitoring for region")
            if CLLocationManager.isMonitoringAvailableForClass(CLCircularRegion) {
                //let r = CLCircularRegion(center: CLLocationCoordinate2D(latitude: 2.0, longitude: 2.0), radius: 10.0, identifier: "2x2 circle")
                //manager.startMonitoringForRegion(r)
            } else {
                println("Region monitoring not available for CLCircularRegion")
            }
            break
        case .AuthorizedWhenInUse:
            fallthrough
        case .Denied:
            fallthrough
        case .Restricted:
            println("Authorization Status is \(status.toString())")
            
            if let url = NSURL(string:UIApplicationOpenSettingsURLString) {
                println("Opening the following Settings URL so you can adjust the permissions: \(url)")
                UIApplication.sharedApplication().openURL(url)
            }
        }
    }
    
    func locationManager(manager: CLLocationManager!, didDetermineState state: CLRegionState, forRegion region: CLRegion!) {
        println("did determine state for region \(region)")
    }
    
    func locationManager(manager: CLLocationManager!, didEnterRegion region: CLRegion!) {
        println("Entered region \(region)")
    }
    
    func locationManager(manager: CLLocationManager!, didExitRegion region: CLRegion!) {
        println("Exit region \(region)")
    }
    
    func locationManager(manager: CLLocationManager!, didFailWithError error: NSError!) {
        println("Fail with error \(error)")
    }
    
    func locationManager(manager: CLLocationManager!, didFinishDeferredUpdatesWithError error: NSError!) {
        println("finished deferred updates with error \(error)")
    }
    
    func locationManager(manager: CLLocationManager!, didRangeBeacons beacons: [AnyObject]!, inRegion region: CLBeaconRegion!) {
        println("didRangeBeacons")
    }
    
    func locationManager(manager: CLLocationManager!, didStartMonitoringForRegion region: CLRegion!) {
        println("Did start monitoring region")
    }
    
    func locationManager(manager: CLLocationManager!, didUpdateHeading newHeading: CLHeading!) {
        println("Did update heading")
    }
    
    func locationManager(manager: CLLocationManager!, didUpdateLocations locations: [AnyObject]!) {
        println("Did update \(locations.count) locations")
        if locations == nil { return }
        for l in locations {
            if let loc = l as? CLLocation {
                println("  location: \(loc)")
                if loc.horizontalAccuracy < 20.0 {
                    println("     Good enough")
                } else {
                    println("     Meh. Crappy accuracy")
                }
            }
        }
        
        println("Most recent location is \(manager.location)")
    }
    
    func locationManager(manager: CLLocationManager!, didUpdateToLocation newLocation: CLLocation!, fromLocation oldLocation: CLLocation!) {
        println("Did update to location")
    }
    
    func locationManager(manager: CLLocationManager!, didVisit visit: CLVisit!) {
        println("Did visit")
    }
    
    func locationManager(manager: CLLocationManager!, monitoringDidFailForRegion region: CLRegion!, withError error: NSError!) {
        println("Monitoring failed for region")
    }
    
    func locationManager(manager: CLLocationManager!, rangingBeaconsDidFailForRegion region: CLBeaconRegion!, withError error: NSError!) {
        println("Ranging beacons did fail for region")
    }
    
    func locationManagerDidPauseLocationUpdates(manager: CLLocationManager!) {
        println("Did pause location update")
    }
    
    func locationManagerDidResumeLocationUpdates(manager: CLLocationManager!) {
        println("Did resume location updates")
    }
    
    func locationManagerShouldDisplayHeadingCalibration(manager: CLLocationManager!) -> Bool {
        println("Should display heading calibaration")
        return false
    }
}
