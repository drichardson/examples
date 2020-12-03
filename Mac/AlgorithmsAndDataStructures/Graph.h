//
//  Graph.h
//  AlgorithmsAndDataStructures
//
//  Created by Douglas Richardson on 3/18/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import <Foundation/NSObject.h>
#import <Foundation/NSSet.h>
#import <Foundation/NSDictionary.h>

@class Vertex;

enum VertexColor
{
	VCWhite,
	VCGray,
	VCBlack
};

@interface GraphSearchResultData : NSObject {
	unsigned mDepth;
	enum VertexColor mColor;
	Vertex *mPredecessor;
}
-(id)initWithDepth:(unsigned)depth color:(enum VertexColor)color predecessor:(Vertex*)predecessor;
-(unsigned)depth;
-(void)setDepth:(unsigned)newDepth;
-(enum VertexColor)color;
-(void)setColor:(enum VertexColor)color;
-(Vertex*)predecessor;
-(void)setPredecessor:(Vertex*)predecessor;
@end

@interface Vertex : NSObject <NSCopying> {
	NSMutableSet *mEdges;
	id mValue;
	id mKey;
	GraphSearchResultData *mSearchData;
}
-(id) initWithValue:(id)value key:(id)key;
-(id) value;
-(id) key;
-(void) setValue:(id)newValue;
-(void) addEdgeTo:(Vertex*)vertex;
-(NSEnumerator*) edges;
-(GraphSearchResultData*)searchData;
-(void)setSearchData:(GraphSearchResultData*)newSearchData;
@end

@interface Graph : NSObject {
	NSMutableDictionary* mVertices;
}
-(Vertex*) createVertex:(id)key value:(id)value;
-(Vertex*) getVertexByKey:(id)key;
-(void) breadthFirstSearch:(Vertex*)root visitor:(id)obj method:(SEL)visitMethod;
@end
