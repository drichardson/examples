/*
 *  Board.h
 *  chess
 *
 *  Created by Doug on 11/16/08.
 *  Copyright 2008 Douglas Richardson. All rights reserved.
 *
 */

#ifndef CHESS_BOARD_H
#define CHESS_BOARD_H

#include "UIObject.h"
#include "Piece.h"
#include <OpenGLES/ES1/gl.h>

class Board	: public UIObject {
	
#pragma pack(1)
	struct BoardVertex
	{
		GLfloat x;
		GLfloat y;
	};
	
	struct BoardTriangle
	{
		BoardVertex v1;
		BoardVertex v2;
		BoardVertex v3;
	};
	
	struct BoardSquare
	{
		BoardTriangle t1;
		BoardTriangle t2;
	};
	
	struct BoardColor
	{
		GLbyte red;
		GLbyte green;
		GLbyte blue;
		GLbyte alpha;
	};
	
#pragma options align=reset
	
protected:
	BoardSquare mBoardSquareTriangles[8][8]; // 8 rows by 8 columns
	BoardColor mBoardSquareTriangleColors[8][8][6]; // 6 colors - one for each verticie in a square.
	
	void renderSquares();
	void renderBorders();
	void renderPieces();
	
public:
	Board();
	void render();
};

#endif