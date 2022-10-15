//
//  actor1.swift
//  actors
//
//  Created by Doug Richardson on 9/11/22.
//

import Foundation

actor Actor1 {
    let immutableString : String
    var mutable : Int = 0
    
    init(string : String) {
        immutableString = string
    }
    
    nonisolated func onlyConstant() -> String {
        return "a constant string"
    }
    
    nonisolated func onlyImmutable() -> String {
        return immutableString
    }
    
    func mutate() {
        mutable += 1
    }
    
    func getMutable() -> Int {
        return mutable
    }
    
    func longOperation() -> Int {
        print("before mutate: \(mutable)")
        mutate()
        print("before after: \(mutable)")
        sleep(1)
        print("after sleep: \(mutable)")
        return mutable
    }
}
