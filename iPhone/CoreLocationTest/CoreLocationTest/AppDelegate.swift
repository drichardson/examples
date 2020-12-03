//
//  AppDelegate.swift
//  CoreLocationTest
//
//  Created by Douglas Richardson on 2/18/15.
//  Copyright (c) 2015 True Labs, Inc. All rights reserved.
//

import UIKit
import CoreLocation

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {

    var window: UIWindow?
    var location = Location()


    func application(application: UIApplication, didFinishLaunchingWithOptions launchOptions: [NSObject: AnyObject]?) -> Bool {
        // Override point for customization after application launch.
        
        NSLog("application:didFinishLaunchingWithOptions:")
        
        if launchOptions?[UIApplicationLaunchOptionsLocationKey] != nil {
            
            // Launching the app in response to a CoreLocation event
            
            NSLog("Launched because of CoreLocation event")
            
            /* To get the region, you have to fire up CLLocationManager and get it delivered
            to the delegate.
            
            This method will be called for all region events (entered/exited)... I think
            any time the region state changes. For that reason, you may want to start location
            manager and figure out what kind of event this is.
            */
            
            /*
            To test this case on device:
            - create and run another application in Xcode.
            - stop this application
            - from Xcode debugging the other application, simulate location to a region this application is monitoring
            At this point, your application should be launched.
            */
            
            let n = UILocalNotification()
            n.userInfo = ["MyKey" : "My Value"]
            n.alertBody = "Region event started app" // in a real app, this should be a localized string key
            n.alertAction = "Region event start app action"
            UIApplication.sharedApplication().presentLocalNotificationNow(n)
            
        }
        
        if let note = launchOptions?[UIApplicationLaunchOptionsLocalNotificationKey] as? UILocalNotification {
            NSLog("Launched because of local notification: \(note)")
        }
        
        // Register for notifications. NOTE: in a real app, you should do this only when you actually
        // need it, because it will display a prompt to the user the first time only. If you request
        // without the user expecting it, they may deny you.
        
        let settings = UIUserNotificationSettings(forTypes: .Alert, categories: nil)
        UIApplication.sharedApplication().registerUserNotificationSettings(settings)
        
        return true
    }

    func applicationWillResignActive(application: UIApplication) {
        // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
        // Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
        
        println("applicationWillResignActive")
    }

    func applicationDidEnterBackground(application: UIApplication) {
        // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later.
        // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
        println("applicationDidEnterBackground")
    }

    func applicationWillEnterForeground(application: UIApplication) {
        // Called as part of the transition from the background to the inactive state; here you can undo many of the changes made on entering the background.
        println("applicationWillEnterForeground")
    }

    func applicationDidBecomeActive(application: UIApplication) {
        // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
        println("applicationDidBecomeActive")
    }

    func applicationWillTerminate(application: UIApplication) {
        // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
        println("applicationWillTerminate")
    }
    
    func application(application: UIApplication, didReceiveLocalNotification notification: UILocalNotification) {
        
        /*
        If the app is foremost and visible when the system delivers the notification, the app delegate’s
        application:didReceiveLocalNotification: is called to process the notification. Use the information
        in the provided UILocalNotification object to decide what action to take. The system does not
        display any alerts, badge the app’s icon, or play any sounds when the app is already frontmost.
        */
        println("didReceiveLocalNotification: \(notification)")
    }

    func application(application: UIApplication, didRegisterUserNotificationSettings notificationSettings: UIUserNotificationSettings) {
        println("didRegisterUserNotificationSettings")
    }

}

