//
//  Simulation.m
//  PhysicsSimulation
//
//  Created by Doug Richardson on 4/11/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "Simulation.h"
#import <OpenGL/OpenGL.h>

@implementation Simulation

- (id)initWithWorldRect:(CGRect)worldRect
{
    self = [super init];
    if (self) {
        
        NSMutableArray* particles = [NSMutableArray array];
        
        _worldRect = worldRect;
        
        for(int i = 0; i < 3000; ++i)
        {
            Particle* p = [Particle new];
            ParticleState s = p.state;
            s.position.x = ((int)(arc4random() % (int)_worldRect.size.width)) - (_worldRect.size.width / 2.0);
            s.position.y = ((int)(arc4random() % (int)_worldRect.size.height)) - (_worldRect.size.height / 2.0);
            s.velocity.x = ((int)(arc4random() % 201)) - 100;
            s.velocity.y = ((int)(arc4random() % 101)) - 50;
            p.state = s;
            [particles addObject:p];
            [p release];
        }
        
        _particles = [particles retain];
        
    }
    
    return self;
}

- (void)dealloc
{
    [_particles release];
    [super dealloc];
}

- (void)setWorldRect:(CGRect)r
{
    _worldRect = r;
}

- (void)update
{
    CFAbsoluteTime now = CFAbsoluteTimeGetCurrent();
    CFAbsoluteTime diff = now - _lastTime;
    
    if (_lastTime > 0)
    {
        [_particles enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
            Particle* particle = (Particle*)obj;
            ParticleState s = particle.state;
            
            Vector3 p = s.position;
            Vector3 v = s.velocity;
            
            if ( p.x < CGRectGetMinX(_worldRect) && v.x < 0 || p.x > CGRectGetMaxX(_worldRect) && v.x > 0 )
            {
                v.x = -v.x;
            }
            
            if ( p.y < CGRectGetMinY(_worldRect) && v.y < 0 || p.y > CGRectGetMaxY(_worldRect) && v.y > 0 )
            {
                v.y = -v.y;
            }
            
            p.x += v.x * diff;
            p.y += v.y * diff;
            p.z += v.z * diff;
            
            s.position = p;
            s.velocity = v;
            
            particle.state = s;
        }];
    }
    
    _lastTime = now;
}

- (void)render
{
    // NOT necessarily run from the main thread.
    
    glClearColor(1, 1, 1, 1);
    glClear( GL_COLOR_BUFFER_BIT );
    
    glColor3f(0, 0, 0);
    glMatrixMode(GL_MODELVIEW);
    
    [_particles enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
        [(Particle*)obj render];
    }];
    
    glFlush();
}

@end
