//
//  Simulation.h
//  PhysicsSimulation
//
//  Created by Doug Richardson on 4/11/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <OpenGL/OpenGL.h>
#import "Particle.h"

/*
 Simulation
 */

@interface Simulation : NSObject {
@private
    NSArray* _particles;
    CFAbsoluteTime _lastTime;
    CGRect _worldRect;
}

- (id)initWithWorldRect:(CGRect)worldRect;

// Update the simulation state based on the current time.
- (void)update;


// Render the simulation to the current Open GL context.
- (void)render;

@end
