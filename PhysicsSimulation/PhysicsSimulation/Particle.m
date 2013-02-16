//
//  Particle.m
//  PhysicsSimulation
//
//  Created by Doug Richardson on 4/13/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "Particle.h"


@implementation Particle

@synthesize state=_state;

- (void)render
{
    const double kRenderedSize = 1;
    
    ParticleState state = self.state;
    
    double left = -kRenderedSize / 2.0 + state.position.x;
    double right = left + kRenderedSize;
    double bottom = -kRenderedSize / 2.0 + state.position.y;
    double top = bottom + kRenderedSize;
    
    glBegin(GL_TRIANGLE_STRIP);
        glVertex2d(left, bottom);
        glVertex2d(left, top);
        glVertex2d(right, bottom);
        glVertex2d(right, top);
    glEnd();
}

@end
