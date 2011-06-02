#ifndef _BLOOPENGINE_H_
#define _BLOOPENGINE_H_

#include "Bitboard.h"

class BloopEngine {
	private:
		bool xboard;
		bool post;
		bool postWhilePondering;
		Color whoseMove;
		Bitboard *board;
	
	public:
		BloopEngine();
		BloopEngine(bool);
		void newGame(Color c);
		void setPosition(std::string);
		void setPost(bool);
		void setPostWhilePondering(bool);
		void play();
		void setMaxDepth(int);
		void makeUserMove(std::string);
};

#endif
