//
//  Units.h
//  PhysicsSimulation
//
//  Created by Doug Richardson on 4/13/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

// All units are in SI base units: meters, kilograms, and seconds.

// Vector3 can be used to measure any 3 dimentional vector of base units. For example:
// 1. If used as a position, each component is the value in meters from the origin of its respective axis.
// 2. If used as a velocity, each component is the value in meters per second in the direction of its respective axis.
typedef struct Vector3 {
    double x, y, z;
} Vector3;