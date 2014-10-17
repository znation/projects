#include <iostream>
#include "BloopEngine.h"

using namespace std;

BloopEngine::BloopEngine() {
  xboard = true;
  whoseMove = WHITE;
}

BloopEngine::BloopEngine(bool useXboard) {
  xboard = useXboard;
  whoseMove = WHITE;
}

void BloopEngine::newGame(Color c) {
  board = new Bitboard(c);
  
  if (!xboard)
    board->printBoard();
}

void BloopEngine::setPosition(std::string position) {
  // uses FEN notation (as in PGN)
}

void BloopEngine::setPost(bool set) {
  post = set;
}

void BloopEngine::setPostWhilePondering(bool set) {
  postWhilePondering = set;
}

void BloopEngine::play() {
  cout << "move " << board->makeBestMove(whoseMove) << endl;
  whoseMove = !whoseMove; // flip colors (computer just went)
}

void BloopEngine::setMaxDepth(int) {

}

void BloopEngine::makeUserMove(std::string move) {
  
  if (xboard) {
    // update board status with move
    board->processUserMove(move);
    whoseMove = !whoseMove; // flip colors (user just went)
    
    cout << "move " << board->makeBestMove(whoseMove) << endl;
    whoseMove = !whoseMove; // flip colors (computer just went)
  }
  else {
    // if only 2 characters, show legal moves
    if (move.length() == 2) {
      cout << "Possible legal moves for " << move << " are:" << endl;
      board->showLegalMoves(move);
    }
    else if (move.length() == 4) {
      board->processUserMove(move);
      whoseMove = !whoseMove; // flip colors (user just went)
      
      string move = board->makeBestMove(whoseMove);
      whoseMove = !whoseMove; // flip colors (computer just went)
      cout << "My move is " << move << ". Board is now:" << endl;
      board->printBoard();
    }
  }
}
