//
//  Particle.h
//  PhysicsSimulation
//
//  Created by Doug Richardson on 4/13/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Units.h"


// Particle contains simulation invariants (i.e. mass) and values that are needed to know where the particle
// is going given no external forces (i.e. velocity)

typedef struct ParticleState
{
    Vector3 position;
    Vector3 velocity;
} ParticleState;

@interface Particle : NSObject
{
}

@property ParticleState state;

- (void)render;

@end