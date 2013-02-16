/*
 *  Board.cpp
 *  chess
 *
 *  Created by Doug on 11/16/08.
 *  Copyright 2008 Douglas Richardson. All rights reserved.
 *
 */

#include "Board.h"
#include <stdint.h>

#import <OpenGLES/ES1/glext.h>


Board::Board()
{
	// Build the board model.
	
	for(int row = 0; row < 8; ++row)
	{
		for(int column = 0; column < 8; ++column)
		{
			mBoardSquareTriangles[row][column].t1.v1.x = 0.0f + column;
			mBoardSquareTriangles[row][column].t1.v1.y = 0.0f + row;
			
			mBoardSquareTriangles[row][column].t1.v2.x = 1.0f + column;
			mBoardSquareTriangles[row][column].t1.v2.y = 1.0f + row;
			
			mBoardSquareTriangles[row][column].t1.v3.x = 0.0f + column;
			mBoardSquareTriangles[row][column].t1.v3.y = 1.0f + row;
			
			// Triangle 2
			mBoardSquareTriangles[row][column].t2.v1.x = 0.0f + column;
			mBoardSquareTriangles[row][column].t2.v1.y = 0.0f + row;
			
			mBoardSquareTriangles[row][column].t2.v2.x = 1.0f + column;
			mBoardSquareTriangles[row][column].t2.v2.y = 0.0f + row;
			
			mBoardSquareTriangles[row][column].t2.v3.x = 1.0f + column;
			mBoardSquareTriangles[row][column].t2.v3.y = 1.0f + row;

			// Colors for this square
			bool isWhiteSquare = ((column + row) % 2) != 0;
			for(int i = 0; i < 6; ++i)
			{
				if(isWhiteSquare)
				{
					mBoardSquareTriangleColors[row][column][i].red = 255;
					mBoardSquareTriangleColors[row][column][i].green = 255;
					mBoardSquareTriangleColors[row][column][i].blue = 255;
				}
				else
				{
					mBoardSquareTriangleColors[row][column][i].red = 0;
					mBoardSquareTriangleColors[row][column][i].green = 0;
					mBoardSquareTriangleColors[row][column][i].blue = 0;
				}
				
				mBoardSquareTriangleColors[row][column][i].alpha = 255;
			}
		}
	}
}

void Board::render()
{
	renderSquares();
	renderBorders();
	renderPieces();
}

void Board::renderSquares()
{
	glLoadIdentity();
	glScalef(0.25, 0.25, 1.0);
	glTranslatef(-4.0, -4.0, 0);
	
	glVertexPointer(2, GL_FLOAT, 0, mBoardSquareTriangles);
	glEnableClientState(GL_VERTEX_ARRAY);

	glColorPointer(4, GL_UNSIGNED_BYTE, 0, mBoardSquareTriangleColors);
	glEnableClientState(GL_COLOR_ARRAY);
	
	glDrawArrays(GL_TRIANGLES, 0, 8 * 8 * 3 * 2);
}

void Board::renderBorders()
{
}

void Board::renderPieces()
{
}

//