//
//  main.swift
//  SwiftClosures
//
//  Created by Douglas Richardson on 2/20/15.
//  Copyright (c) 2015 True Labs, Inc. All rights reserved.
//

import Foundation
import Dispatch

println("Hello, World!")

let xConst = 10
var xVar = 20

func printGlobals() {
    println("xConst=\(xConst), xVar=\(xVar)")
}

let timeout = dispatch_time(DISPATCH_TIME_NOW, Int64(2 * NSEC_PER_SEC))
dispatch_after(timeout, dispatch_get_main_queue(), {
    printGlobals()
//    xConst = 2
    xVar = 30
    printGlobals()
})

func doIt() {
    let yConst = 1
    var yVar = 1
    
    let to = dispatch_time(DISPATCH_TIME_NOW, Int64(2 * NSEC_PER_SEC))
    dispatch_after(to, dispatch_get_main_queue(), {
//        yConst = 2
//        yVar = 2
        println("1: yConst = \(yConst), yVar=\(yVar)")
        yVar = 2
    })
    
    let to2 = dispatch_time(to, Int64(2 * NSEC_PER_SEC))
    dispatch_after(to2, dispatch_get_main_queue(), {
        println("2: yConst = \(yConst), yVar=\(yVar)")
    })
}

doIt()

dispatch_main()