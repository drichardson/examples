/* Copyright (c) 2007 Scott Lembcke
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
 
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "chipmunk.h"

#define SLEEP_TICKS 16

extern cpSpace *space;
extern cpBody *staticBody;

// Iterate over all of the bodies and reset the ones that have fallen offscreen.
static void
eachBody(cpBody *body, void *unused)
{
	if(body->p.y < -260 || fabsf(body->p.x) > 340){
		cpFloat x = rand()/(cpFloat)RAND_MAX*640 - 320;
		body->p = cpv(x, 260);
	}
}

void demo3_update(int ticks)
{
	int steps = 1;
	cpFloat dt = 1.0/60.0/(cpFloat)steps;
	
	for(int i=0; i<steps; i++){
		cpSpaceStep(space, dt);
		cpSpaceEachBody(space, &eachBody, NULL);
	}
}

#define NUM_VERTS 5

void demo3_init(void)
{
	staticBody = cpBodyNew(INFINITY, INFINITY);
	
	cpResetShapeIdCounter();
	
	space = cpSpaceNew();
	space->iterations = 5;
	space->gravity = cpv(0, -100);
	
	cpSpaceResizeStaticHash(space, 40.0, 999);
	cpSpaceResizeActiveHash(space, 30.0, 2999);
	
	cpBody *body;
	cpShape *shape;
	
	// Create vertexes for a pentagon shape.
	cpVect verts[NUM_VERTS];
	for(int i=0; i<NUM_VERTS; i++){
		cpFloat angle = -2*M_PI*i/((cpFloat) NUM_VERTS);
		verts[i] = cpv(10*cos(angle), 10*sin(angle));
	}
	
	// Vertexes for a triangle shape.
	cpVect tris[] = {
		cpv(-15,-15),
		cpv(  0, 10),
		cpv( 15,-15),
	};

	// Create the static triangles.
	for(int i=0; i<9; i++){
		for(int j=0; j<6; j++){
			cpFloat stagger = (j%2)*40;
			cpVect offset = cpv(i*80 - 320 + stagger, j*70 - 240);
			shape = cpPolyShapeNew(staticBody, 3, tris, offset);
			shape->e = 1.0; shape->u = 1.0;
			cpSpaceAddStaticShape(space, shape);
		}
	}
	
	// Add lots of pentagons.
	for(int i=0; i<300; i++){
		body = cpBodyNew(1.0, cpMomentForPoly(1.0, NUM_VERTS, verts, cpvzero));
		cpFloat x = rand()/(cpFloat)RAND_MAX*640 - 320;
		body->p = cpv(x, 350);
		
		cpSpaceAddBody(space, body);
		shape = cpPolyShapeNew(body, NUM_VERTS, verts, cpvzero);
		shape->e = 0.0; shape->u = 0.4;
		cpSpaceAddShape(space, shape);
	}
}
