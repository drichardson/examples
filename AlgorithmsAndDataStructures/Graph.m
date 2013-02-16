//
//  Graph.m
//  AlgorithmsAndDataStructures
//
//  Created by Douglas Richardson on 3/18/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import "Graph.h"
#import "Queue.h"
#import <Foundation/NSEnumerator.h>
#import <Foundation/NSString.h>

static const int INITIAL_EDGE_CAPACITY = 1;
static const int INITIAL_VERTEX_CAPACITY = 20;

@implementation Vertex

-(id) initWithValue:(id)value key:(id)key
{
	self = [super init];
	if(self) {
		mEdges = [[NSMutableSet alloc] initWithCapacity:INITIAL_EDGE_CAPACITY];
		[self setValue:value];
		mKey = [key retain];
	}
	return self;
}

-(void)dealloc
{
	[self setValue:nil];
	[mKey release];
	[self setSearchData:nil];
	[super dealloc];
}

-(NSString*)description
{	
	return [NSString stringWithFormat:@"value = {%@}, key = {%@}",
		[mValue description], [mKey description]];
}

-(id) value { return mValue; }
-(id) key { return mKey; }
-(void) setValue:(id)newValue
{
	if(mValue != newValue) {
		[mValue release];
		mValue = [newValue retain];
	}
}

-(void) addEdgeTo:(Vertex*)vertex
{
	[mEdges addObject:vertex];
}

-(NSEnumerator*) edges
{
	return [mEdges objectEnumerator];
}

-(GraphSearchResultData*)searchData { return mSearchData; }
-(void)setSearchData:(GraphSearchResultData*)newSearchData
{
	if(mSearchData != newSearchData) {
		[mSearchData release];
		mSearchData = [newSearchData retain];
	}
}

-(id)copyWithZone:(NSZone*)zone
{
	Vertex *copy = [[[self class] allocWithZone:zone]
		initWithValue:[self value] key:[self key]];
	
	return copy;
}

@end

@implementation GraphSearchResultData
-(id)initWithDepth:(unsigned)depth color:(enum VertexColor)color
	   predecessor:(Vertex*)predecessor
{
	self = [super init];
	if(self) {
		mDepth = depth;
		mColor = color;
		mPredecessor = predecessor;
	}
	return self;
}

-(void)dealloc
{
	[mPredecessor release];
	mPredecessor = nil;
	[super dealloc];
}

-(unsigned)depth { return mDepth; }
-(void)setDepth:(unsigned)newDepth { mDepth = newDepth; }
-(enum VertexColor)color { return mColor; }
-(void)setColor:(enum VertexColor)color
{
	mColor = color;
}
-(Vertex*)predecessor { return mPredecessor; }
-(void)setPredecessor:(Vertex*)predecessor
{
	if(mPredecessor != predecessor) {
		[mPredecessor release];
		mPredecessor = [predecessor retain];
	}
}
@end

@implementation Graph
-(id) init
{
	self = [super init];
	if(self) {
		mVertices = [[NSMutableDictionary alloc] initWithCapacity:INITIAL_VERTEX_CAPACITY];
	}
	return self;
}

-(void) dealloc
{
	[mVertices release];
	mVertices = nil;
	[super dealloc];
}

-(Vertex*) createVertex:(id)key value:(id)value
{
	Vertex *v = [[Vertex alloc] initWithValue:value key:key];
	[mVertices setObject:v forKey:key];
	[v release];
	return v;
}

-(Vertex*) getVertexByKey:(id)key
{
	return [mVertices objectForKey:key];
}

-(void) breadthFirstSearch:(Vertex*)root visitor:(id)obj method:(SEL)visitMethod
{
	NSEnumerator *e = [mVertices objectEnumerator];
	Vertex *v;
	while(v = [e nextObject]) {
		if(![v isEqual:root]) {
			GraphSearchResultData *data = [[GraphSearchResultData alloc]
				initWithDepth:-1 color:VCWhite predecessor:nil];
			[v setSearchData:data];
			[data release];
		}
	}
	
	GraphSearchResultData *data = [[GraphSearchResultData alloc] initWithDepth:0 color:VCGray predecessor:nil];
	[root setSearchData:data];
	[data release];
	
	Queue *q = [[Queue alloc] init];
	[q enqueue:root];
	
	Vertex *u;
	while(u = [q dequeue]) {
		NSEnumerator *edges = [u edges];
		Vertex *v;
		while(v = [edges nextObject]) {
			GraphSearchResultData *data = [v searchData];
			if([data color] == VCWhite) {
				[data setColor:VCGray];
				[data setDepth:[[u searchData] depth] + 1];
				[data setPredecessor:u];
				[q enqueue:v];
			}
		}
		
		[obj performSelector:visitMethod withObject:u];
		[[u searchData] setColor:VCBlack];
	}
	
	[q release];
}

@end
