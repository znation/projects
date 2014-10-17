// TODO: use rotated bitboards for diagonal sliding pieces
// TODO: implement/fix castling
// TODO: fix en passant
// TODO: fix check detection
// TODO: pick random move from among best if >1 with best rating
// TODO: implement pawn promotion
// WHY WON'T IT CAPTURE!?!???

#include <iostream>
#include <math.h>
#include <assert.h>
#include <cstdlib>  // For srand() and rand()
#include <vector>
#include <map>
#include <float.h>
#include <unistd.h> // for usleep()
#include "Bitboard.h"
#include "Constants.h"
#include "GameTreeNode.h"

// "l" index is a hack to store legal moves temporarily
static char pieceNames[] = {'p', 'k', 'q', 'n', 'b', 'r', 'P', 'K', 'Q', 'N', 'B', 'R', 'l'};
static double pieceValues[] = {1, 1000 ,  9 , 3.5, 3.5,  5 ,  -1,  1000,  -9, -3.5,-3.5,  -5,   0 };

int Bitboard::depth = 1;
int Bitboard::numThreads = 1;

static unsigned long long startingBoard[] = {
	0x00FF000000000000LLU, // p
	0x0800000000000000LLU, // k
	0x1000000000000000LLU, // q
	0x4200000000000000LLU, // n
	0x2400000000000000LLU, // b
	0x8100000000000000LLU, // r
	0x000000000000FF00LLU, // P
	0x0000000000000008LLU, // K
	0x0000000000000010LLU, // Q
	0x0000000000000042LLU, // N
	0x0000000000000024LLU, // B
	0x0000000000000081LLU, // R
	0LLU // legal moves (hack)
};

// rotated bitboard indices
/*
static int rotate45R[64] = {
	0, 2, 5, 9, 14, 20, 27, 35,
	1, 4, 8, 13, 19, 26, 34, 42,
	3, 7, 12, 18, 25, 33, 41, 48,
	6, 11, 17, 24, 32, 40, 47, 53,
	10, 16, 23, 31, 39, 46, 52, 57,
	15, 22, 30, 38, 45, 51, 56, 60,
	21, 29, 37, 44, 50, 55, 59, 62,
	28, 36, 43, 49, 54, 58, 61, 63                         
};                      
static int rotate45L[64] = {
	28, 36, 43, 49, 54, 58, 61, 63,
	21, 29, 37, 44, 50, 55, 59, 62,
	15, 22, 30, 38, 45, 51, 56, 60,
	10, 16, 23, 31, 39, 46, 52, 57,
	6,  11, 17, 24, 32, 40, 47, 53,
	3,  7, 12, 18, 25, 33, 41, 48,
	1,  4, 8, 13, 19, 26, 34, 42,
	0,  2, 5, 9, 14, 20, 27, 35
};
*/
static int rotate90R[64] = {
   56, 48, 40, 32, 24, 16, 8,  0,
   57, 49, 41, 33, 25, 17, 9,  1,
   58, 50, 42, 34, 26, 18, 10, 2,
   59, 51, 43, 35, 27, 19, 11, 3,
   60, 52, 44, 36, 28, 20, 12, 4,
   61, 53, 45, 37, 29, 21, 13, 5,
   62, 54, 46, 38, 30, 22, 14, 6,
   63, 55, 47, 39, 31, 23, 15, 7
};


using namespace std;

void Bitboard::setupBoard() {
  int i;
  
  zeroOutBoard();
  
  for (i=0; i<13; i++) {
    // for each piece, set it on each rotated board
    int j;
    for (j=0; j<64; j++) {
      if (startingBoard[i] & indexToBits[j]) {
        // hit a piece at this index
        togglePiece((piece) i, indexToBits[j]);
      }
    }
  }
}

Bitboard::~Bitboard() {
  delete previousMove;
}

// makes move "move"
Bitboard::Bitboard(Bitboard old, Move move) {
  //printf("DEBUG: Copying an old bitboard from an object and making a move.\n");
  // copy the old bitboard
  zeroOutBoard();
  int i;
  for (i=0; i<12; i++) {
    board[i] = old.board[i];
    board_r90[i] = old.board_r90[i];
    //board_ld45[i] = old.board_ld45[i];
    //board_td45[i] = old.board_td45[i];
  }
  
  myColor = !old.myColor;
  
  // make the move
  piece xyz = (piece) move.getPiece();
  togglePiece(xyz, move.getFrom(), move.getTo());
  
  whiteHasMovedKing = old.whiteHasMovedKing;
  blackHasMovedKing = old.blackHasMovedKing;
  whiteHasMovedARook = old.whiteHasMovedARook;
  blackHasMovedARook = old.blackHasMovedARook;
  whiteHasMovedHRook = old.whiteHasMovedHRook;
  blackHasMovedHRook = old.blackHasMovedHRook;
  
  previousMove = new Move(move);
}

// makes the move in the arguments
Bitboard::Bitboard(Bitboard *old, unsigned int xyz, unsigned long long from, unsigned long long to) {
  //printf("DEBUG: Copying an old bitboard from a pointer and making a move.\n");
  // copy the old bitboard
  zeroOutBoard();
  int i;
  for (i=0; i<12; i++) {
    board[i] = old->board[i];
    board_r90[i] = old->board_r90[i];
    //board_ld45[i] = old->board_ld45[i];
    //board_td45[i] = old->board_td45[i];
  }
  
  myColor = !old->myColor;
  
  // make the move
  togglePiece((piece) xyz, from, to);
  
  whiteHasMovedKing = old->whiteHasMovedKing;
  blackHasMovedKing = old->blackHasMovedKing;
  whiteHasMovedARook = old->whiteHasMovedARook;
  blackHasMovedARook = old->blackHasMovedARook;
  whiteHasMovedHRook = old->whiteHasMovedHRook;
  blackHasMovedHRook = old->blackHasMovedHRook;
  
  previousMove = new Move(xyz, from, to);
}

Bitboard::Bitboard(Color c) : previousMove(NULL) {
  // initializing new bitboard
  
  whiteHasMovedKing = false;
  blackHasMovedKing = false;
  whiteHasMovedARook = false;
  blackHasMovedARook = false;
  whiteHasMovedHRook = false;
  blackHasMovedHRook = false;

  // debug diagonals
  /*
  unsigned char i;
  cout << "--------" << endl;
  for (i=0; i<64; i++) {
    unsigned long long bb = indexToBits[i];
    printf("%d\t", findDiagonal(getRankNumber(bb), getFileNumber(bb)));
    if ((i+1)%8 == 0)
      printf("\n");
  }
  cout << "--------" << endl;
  assert(0==1);
  */

  myColor = c;

  // initialize pieces on the board
  setupBoard();
  
  // assertions
  assert(squareToString(board[k]) == "e1");
  assert(squareToString(board[Q]) == "d8");
  assert(stringToSquare("e1") == board[k]);
  assert(stringToSquare("d8") == board[Q]);
  assert(bitsToIndex(stringToSquare("e1")) == bitsToIndex(indexToBits[bitsToIndex(stringToSquare("e1"))]));
  assert(bitsToIndex(stringToSquare("d8")) == bitsToIndex(indexToBits[bitsToIndex(stringToSquare("d8"))]));
  
  assert(bitsToIndex(stringToSquare("h1")) == 56);
  
  // make sure rotate90R works
  assert(rotate90R[rotate90R[rotate90R[rotate90R[bitsToIndex(board[k])]]]] 
    == bitsToIndex(board[k]));
    
  // make sure countPieces works
  assert(countPieces(board[k]) == 1);
  assert(countPieces(board[p]) == 8);
  assert(countPieces(board[R]) == 2);
  
  // make sure bitsFromRankAndFile works
  assert(bitsFromRankAndFile(getRankNumber(board[k]), getFileNumber(board[k])) == board[k]);
  
  assert(rank_attacks[56][255] & stringToSquare("g1"));
  
  assert(bitsFromRankAndFile(7, 7) == stringToSquare("a1"));
  
  // test game tree node class
  /*
  vector<GameTreeNode *> * nodesToExpand = new vector<GameTreeNode *>();
  
  GameTreeNode * rootNode = new GameTreeNode( // root node is the already made move
  								new Bitboard(
  									this,
  									p,
  									stringToSquare("e2"),
  									stringToSquare("e4")),
  								3, // num of ply
  								BLACK,
  								stringToSquare("e2"),
  								stringToSquare("e4"));
  nodesToExpand->push_back(rootNode);
  
  GameTreeNode::expandGameTree(nodesToExpand);
  
  delete nodesToExpand;
  
  GameTreeNode::debugGameTree(rootNode);
  
  // should delete the entire tree
  delete rootNode;
  */
  
}

unsigned long long Bitboard::white() {
  return (board[k] | board[q] | board[r] | board[b] | board[n] | board[p]);
}

unsigned long long Bitboard::black() {
  return (board[K] | board[Q] | board[R] | board[B] | board[N] | board[P]);
}

unsigned long long Bitboard::occupied() {
  return (white() | black());
}

unsigned long long Bitboard::occupied_r90() {
  int i;
  unsigned long long occupied = 0LLU;
  for (i=0; i<12; i++) {
    occupied |= board_r90[i];
  }
  return occupied;
}

/*
unsigned long long Bitboard::occupied_ld45() {
  int i;
  unsigned long long occupied = 0LLU;
  for (i=0; i<12; i++) {
    occupied |= board_ld45[i];
  }
  return occupied;
}

unsigned long long Bitboard::occupied_td45() {
  int i;
  unsigned long long occupied = 0LLU;
  for (i=0; i<12; i++) {
    occupied |= board_td45[i];
  }
  return occupied;
}
*/

unsigned long long Bitboard::empty() {
  return (~occupied());
}

unsigned long long Bitboard::getSinglePieceMoves(Color c, unsigned int xyz, unsigned long long bb) {
  return getSinglePieceMoves(c, (piece) xyz, bb);
}

// shows castling moves
unsigned long long Bitboard::getSinglePieceMoves(Color c, piece xyz, unsigned long long bb) {
  assert(countPieces(bb) <= 1);
  
  switch (xyz) {
    case p: assert(c == WHITE); return getPawnMoves(c, bb);
    case P: assert(c == BLACK); return getPawnMoves(c, bb);
    case r: assert(c == WHITE); return getRookMoves(c, bb);
    case R: assert(c == BLACK); return getRookMoves(c, bb);
    case n: assert(c == WHITE); return getKnightMoves(c, bb);
    case N: assert(c == BLACK); return getKnightMoves(c, bb);
    case b: assert(c == WHITE); return getBishopMoves(c, bb);
    case B: assert(c == BLACK); return getBishopMoves(c, bb);
    case q: assert(c == WHITE); return getQueenMoves(c, bb);
    case Q: assert(c == BLACK); return getQueenMoves(c, bb);
    case k: assert(c == WHITE); return getKingMoves(c, bb) | getCastlingMoves(c);
    case K: assert(c == BLACK); return getKingMoves(c, bb) | getCastlingMoves(c);
    case l: return 0LLU; // ignore the legal-move-displaying hack
  }
  
  return 0LLU;
}

/*
unsigned long long Bitboard::getMoves(Color c, piece xyz) {
  switch (xyz) {
    case p: return getPawnMoves(c, board[p]);
    case P: return getPawnMoves(c, board[P]);
    case r: return getRookMoves(c, board[r]);
    case R: return getRookMoves(c, board[R]);
    case n: return getKnightMoves(c, board[n]);
    case N: return getKnightMoves(c, board[N]);
    case b: return getBishopMoves(c, board[b]);
    case B: return getBishopMoves(c, board[B]);
    case q: return getQueenMoves(c, board[q]);
    case Q: return getQueenMoves(c, board[Q]);
    case k: return getKingMoves(c, board[k]);
    case K: return getKingMoves(c, board[K]);
    case l: return 0LLU; // ignore the legal-move-displaying hack
  }
  
  return 0LLU;
}
*/

// doesn't show castling moves
unsigned long long Bitboard::getAllMoves(Color c) {
  unsigned long long moves = 0LLU;
  moves |= getPawnMoves(c, board[c == WHITE ? p : P]);
  moves |= getRookMoves(c, board[c == WHITE ? r : R]);
  moves |= getKnightMoves(c, board[c == WHITE ? n : N]);
  moves |= getBishopMoves(c, board[c == WHITE ? b : B]);
  moves |= getQueenMoves(c, board[c == WHITE ? q : Q]);
  moves |= getKingMoves(c, board[c == WHITE ? k : K]);
  return moves;
}


unsigned long long Bitboard::getPawnMoves(Color c, unsigned long long pawn) {
  if (c == BLACK) {
  	unsigned long long forward = (pawn << 8) & empty();
  	unsigned long long forward2 = ((((pawn & rank7) << 8) & empty()) << 8) & empty();
  	unsigned long long captureRight = ((pawn & ~fileh) << 7) & white();
  	unsigned long long captureLeft = ((pawn & ~filea) << 9) & white();
  	unsigned long long enPassant = 0LLU;
  	
  	// check for en passant
  	if (previousMove != NULL && previousMove->getPiece() == p) {
  	  unsigned long long white_forward2 = (((((previousMove->getFrom() & rank2) >> 8) & empty()) >> 8) & empty());
  	  if (white_forward2 & previousMove->getTo()) {
  	    // white made a forward2 move
  	    cerr << "DEBUG: possibility of en passant." << endl;
  	    
  	    // record where the pawn was in the middle of it
  	    unsigned long long movingTarget = previousMove->getTo() << 8;
  	    
  	    // capture en passant right
  	    enPassant |= ((pawn & ~fileh) << 7) & movingTarget;
  	    
  	    // capture en passant left
  	    enPassant |= ((pawn & ~filea) << 9) & movingTarget;
  	  }
  	}
  	
  	return (forward | forward2 | captureRight | captureLeft | enPassant);
  }
  
  else {
    // c is white
    unsigned long long forward = (pawn >> 8) & empty();
    unsigned long long forward2 = ((((pawn & rank2) >> 8) & empty()) >> 8) & empty();
    unsigned long long captureRight = ((pawn & ~fileh) >> 9) & black();
    unsigned long long captureLeft = ((pawn & ~filea) >> 7) & black();
    unsigned long long enPassant = 0LLU;
    
    // check for en passant
    if (previousMove != NULL && previousMove->getPiece() == P) {
      unsigned long long black_forward2 = ((((previousMove->getFrom() & rank7) << 8) & empty()) << 8) & empty();
      if (black_forward2 & previousMove->getTo()) {
        // black made a forward2 move
        cerr << "DEBUG: possibility of en passant." << endl;
        
        // record where the pawn was in the middle of it
        unsigned long long movingTarget = previousMove->getTo() >> 8;
        
        // capture en passant right
        enPassant |= ((pawn & ~fileh) >> 9) & movingTarget;
        
        // capture en passant left
        enPassant |= ((pawn & ~filea) >> 7) & movingTarget;
      }
    }
    
    return (forward | forward2 | captureRight | captureLeft | enPassant);
  }
}

unsigned long long Bitboard::getRookMoves(Color c, unsigned long long rook) {
  // can only operate on one rook at a time!
  
  unsigned char numRooks = countPieces(rook);
  
  if (numRooks == 0) {
    return 0LLU; // 0 rooks = no moves
  }
  
  else if (numRooks > 1) {
    // separate them and combine the result
    unsigned long long * pieces = separatePieces(rook);
    unsigned char i;
    unsigned long long result = 0LLU;
    for (i=0; i<64; i++) {
      result |= getRookMoves(c, pieces[i]);
    }
    return result;
  }
  
  // base case: 1 rook to move
  
  // find the rank and file the rook is on
  unsigned long long rankContents, fileContents;
  
  rankContents = (occupied() >> (getRankNumber(rook) * 8)) & 0xFF;
  fileContents = (occupied_r90() >> ((7-getFileNumber(rook)) * 8)) & 0xFF;
  
  unsigned long long moves = 0LLU;
  
  unsigned char idx = bitsToIndex(rook);
  
  moves |= rank_attacks[idx][rankContents];
  moves |= file_attacks[idx][fileContents];
  
  
  
  /*
  cout << "---DEBUG---" << endl;
  
  cout << "rank is " << getRankNumber(rook) << ", file is " << getFileNumber(rook) << endl;
  cout << "rook is:" << endl;
  debugBoard(rook);
  
  cout << "rank contents:" << endl;
  debugBoard(rankContents);
  
  cout << "file contents:" << endl;
  debugBoard(fileContents);
  
  cout << "rank attacks:" << idx << " " << rankContents << endl;
  debugBoard(rank_attacks[idx][rankContents]);
  
  cout << "file attacks:" << idx << " " << fileContents << endl;
  debugBoard(file_attacks[idx][fileContents]);
  
  */
  

  /*
  cout << "moves:" << endl;
  debugBoard(moves);
  */

  if (c == BLACK) {
    return (moves & ~black());
  }
  else {
    return (moves & ~white());
  }
}

unsigned long long Bitboard::getKnightMoves(Color c, unsigned long long bb) {
  // TODO: optimize this using array lookup ...?
  unsigned long long moves =
    (bb & ~ (rank1 | filegh)) << 6  |
    (bb & ~ (rank8 | filegh)) >> 10 |
    (bb & ~ (rank1 | fileab)) << 10 |
    (bb & ~ (rank8 | fileab)) >> 6  |
    (bb & ~ (rank12 | fileh)) << 15 |
    (bb & ~ (rank12 | filea)) << 17 |
    (bb & ~ (rank78 | fileh)) >> 17 |
    (bb & ~ (rank78 | filea)) >> 15;
  if (c == BLACK) {
    return (moves & ~black());
  }
  else {
    return (moves & ~white());
  }
}

unsigned long long Bitboard::getBishopMoves(Color c, unsigned long long bishop) {
  /*
  unsigned char numBishops = countPieces(bishop);
  
  if (numBishops == 0) {
    return 0LLU; // 0 rooks = no moves
  }
  
  else if (numBishops > 1) {
    // separate them and combine the result
    unsigned long long * pieces = separatePieces(bishop);
    unsigned char i;
    unsigned long long result = 0LLU;
    for (i=0; i<64; i++) {
      result |= getBishopMoves(c, pieces[i]);
    }
    return result;
  }
  
  // base case: 1 bishop to move
  
  unsigned long long ld45_contents = 0LLU;
  
  // find diagonal
  
  
  // get appropriate bits out of the diagonal occupied bitboard
  unsigned char rank = getRankNumber(bishop);
  unsigned char file = getFileNumber(bishop);
  int diagonal = (7 + findDiagonal_a1h8(rank, file));
  
  //cout << "DEBUG: diagonal is " << diagonal << endl;

  ld45_contents = (occupied_ld45() >> diagonalShiftAmount[diagonal]) & diagonalShiftMask[diagonal];
  
  //debugBoard(ld45_contents);
  
  unsigned long long moves = 0LLU;
  
  unsigned char idx = bitsToIndex(bishop);
  
  moves |= diaga1h8_attacks[idx][ld45_contents];

  if (c == BLACK) {
    return (moves & ~black());
  }
  else {
    return (moves & ~white());
  }
  */
  
  // calculate the SLOW way
  unsigned char numBishops = countPieces(bishop);
  
  if (numBishops == 0) {
    return 0LLU; // 0 rooks = no moves
  }
  
  else if (numBishops > 1) {
    // separate them and combine the result
    unsigned long long * pieces = separatePieces(bishop);
    unsigned char i;
    unsigned long long result = 0LLU;
    for (i=0; i<64; i++) {
      result |= getBishopMoves(c, pieces[i]);
    }
    return result;
  }
  
  // base case: 1 bishop to move
  unsigned long long moves = 0LLU;

  unsigned char rank = getRankNumber(bishop);
  unsigned char file = getFileNumber(bishop);

  char i = rank+1;
  char j = file+1;
  while (i < 8 && j < 8) {
    assert(i >= 0);
    assert(j >= 0);
    assert(i < 8);
    assert(j < 8);
    unsigned long long position = bitsFromRankAndFile(i, j);
    if (position & occupied()) {
      if (c == WHITE)
        if (position & black())
          moves |= position;
      else
        if (position & white())
          moves |= position;
      break;
    }
    moves |= position;
    i++;
    j++;
  }
  
  i = rank-1;
  j = file-1;
  while (i >= 0 && j >= 0) {
    assert(i >= 0);
    assert(j >= 0);
    assert(i < 8);
    assert(j < 8);
    unsigned long long position = bitsFromRankAndFile(i, j);
    if (position & occupied()) {
      if (c == WHITE)
        if (position & black())
          moves |= position;
      else
        if (position & white())
          moves |= position;
      break;
    }
    moves |= position;
    i--;
    j--;
  }
  
  i = rank-1;
  j = file+1;
  while (i >=0 && j < 8) {
    assert(i >= 0);
    assert(j >= 0);
    assert(i < 8);
    assert(j < 8);
    unsigned long long position = bitsFromRankAndFile(i, j);
    if (position & occupied()) {
      if (c == WHITE)
        if (position & black())
          moves |= position;
      else
        if (position & white())
          moves |= position;
      break;
    }
    moves |= position;
    i--;
    j++;
  }
  
  i = rank+1;
  j = file-1;
  while (i < 8 && j >= 0) {
    assert(i >= 0);
    assert(j >= 0);
    assert(i < 8);
    assert(j < 8);
    unsigned long long position = bitsFromRankAndFile(i, j);
    moves |= position;
    if (position & occupied())
      break;
    
    i++;
    j--;
  }
  
  if (c == BLACK) {
    return (moves & ~black());
  }
  else {
    return (moves & ~white());
  }
}

unsigned long long Bitboard::getQueenMoves(Color c, unsigned long long queen) {
  // TODO: add diagonal moves to queen
  // can only operate on one queen at a time!
  
  unsigned char numQueens = countPieces(queen);
  
  if (numQueens == 0) {
    return 0LLU; // 0 rooks = no moves
  }
  
  else if (numQueens > 1) {
    // separate them and combine the result
    unsigned long long * pieces = separatePieces(queen);
    unsigned char i;
    unsigned long long result = 0LLU;
    for (i=0; i<64; i++) {
      result |= getQueenMoves(c, pieces[i]);
    }
    return result;
  }
  
  // base case: 1 queen to move
  
  /*
  // find the rank and file the queen is on
  unsigned long long rankContents, fileContents;
  
  rankContents = (occupied() >> (getRankNumber(queen) * 8)) & 0xFF;
  fileContents = (occupied_r90() >> ((7-getFileNumber(queen)) * 8)) & 0xFF;
  
  unsigned long long moves = 0LLU;
  
  unsigned char idx = bitsToIndex(queen);
  
  moves |= rank_attacks[idx][rankContents];
  moves |= file_attacks[idx][fileContents];
  */
  
  // hack method
  unsigned long long moves = getRookMoves(c, queen);
  moves |= getBishopMoves(c, queen);
  
  if (c == BLACK) {
    return (moves & ~black());
  }
  else {
    return (moves & ~white());
  }
  
}

// returns king moves for castling
unsigned long long Bitboard::getCastlingMoves(Color c) {
  // use the whole king and whole rook bb
  if (canCastle(c)) {
    if (c == WHITE) {
      unsigned long long castlingMoves = 0LLU;
      if (!whiteHasMovedARook) {
        // check each square between a and e
        if (!(stringToSquare("b1") & white()))
          if (!(stringToSquare("c1") & white()))
            if (!(stringToSquare("c1") & getAllMoves(BLACK))) // would put king in check
              if (!(stringToSquare("d1") & white()))
                if (!(stringToSquare("d1") & getAllMoves(BLACK))) // would put the king in check
                  castlingMoves |= stringToSquare("c1");
            
      }
      if (!whiteHasMovedHRook) {
        // check each square between e and h
        if (!(stringToSquare("f1") & white()))
          if (!(stringToSquare("f1") & getAllMoves(BLACK)))
            if (!(stringToSquare("g1") & white()))
              if (!(stringToSquare("g1") & getAllMoves(BLACK)))
                castlingMoves |= stringToSquare("g1");
      }
      return castlingMoves;
    }
    else {
      unsigned long long castlingMoves = 0LLU;
      if (!blackHasMovedARook) {
      // check each square between a and e
      if (!(stringToSquare("b8") & black()))
          if (!(stringToSquare("c8") & black()))
            if (!(stringToSquare("c8") & getAllMoves(WHITE))) // would put king in check
              if (!(stringToSquare("d8") & black()))
                if (!(stringToSquare("d8") & getAllMoves(WHITE))) // would put the king in check
                  castlingMoves |= stringToSquare("c8");
      }
      if (!blackHasMovedHRook) {
      // check each square between e and h
        if (!(stringToSquare("f8") & black()))
          if (!(stringToSquare("f8") & getAllMoves(WHITE)))
            if (!(stringToSquare("g8") & black()))
              if (!(stringToSquare("g8") & getAllMoves(WHITE)))
                castlingMoves |= stringToSquare("g8");
      }
      return castlingMoves;
    }
  }
  
  return 0LLU;
}

unsigned long long Bitboard::getKingMoves(Color c, unsigned long long bb) {
  // TODO: optimize this using array lookup ...?
  unsigned long long moves = (bb & ~ rank8)
    >> 8 |  (bb & ~ rank1) << 8 |
    (bb & ~ fileh) >> 1 | (bb & ~
    filea) << 1 | (bb & ~ ( rank8 |
    filea)) >> 7 | (bb & ~ ( rank8 |
    fileh)) >> 9 |  (bb & ~ (rank1 |
    fileh )) << 7 | (bb & ~ (rank1 |
    filea )) << 9;
  if (c == BLACK) {
    return (moves & ~black());
  }
  else {
    return (moves & ~white());
  }
}

unsigned long long Bitboard::pickRandomOccupiedSquare(unsigned long long bb) {
  vector<unsigned long long> occupied;
  unsigned long long i;
  for (i=0; i<64; i++) {
  	unsigned long long mask = 1LLU << i;
  	if (mask & bb)
  	  occupied.push_back(mask);
  }
  if (occupied.size() != 0) {
    int choice = rand() % occupied.size();
    return occupied.at(choice);
  }
  // no occupied squares
  return 0;
}

/*
string Bitboard::makeRandomMove(Color c) {
  // get a piece
  unsigned long long piece;
  switch (c) {
    case WHITE:
      // TODO: make this work for white
      break;
    case BLACK:
      piece = pickRandomOccupiedSquare(board[P] | board[N] | board[K]);
      break;
  }
  
  assert(piece != 0);
  
  // see legal moves
  unsigned long long moves;
  if (board[P] & piece)
    moves = getPawnMoves(c, piece);
  else if (board[N] & piece)
    moves = getKnightMoves(c, piece);
  else if (board[K] & piece)
    moves = getKingMoves(c, piece);
  
  assert(moves != 0);
  
  // pick a random legal move
  
  unsigned long long move = pickRandomOccupiedSquare(moves);
  
  // move the piece in the actual bitboard
  unsigned long long *pointer;
  if (board[P] & piece)
    pointer = &board[P];
  else if (board[N] & piece)
    pointer = &board[N];
  else if (board[K] & piece)
    pointer = &board[K];
  
  assert(pointer != NULL);
  
  // TODO use togglePiece here!!!
  *pointer ^= piece;
  *pointer |= move;
  
  return squareToString(piece) + squareToString(move);
}
*/

string Bitboard::squareToString(unsigned long long bb) {
  int row = 7-getRankNumber(bb);
  int col = 7-getFileNumber(bb);
  
  char square[3];
  square[0] = col + 97; // column/file (letter)
  square[1] = row + 49; // row/rank (number)
  square[2] = '\0';
  
  //cout << "DEBUG: square is (" << square << ")" << endl;
  return string(square);
}

unsigned long long Bitboard::stringToSquare(string bb) {
  const char *bbChars = bb.c_str();

  unsigned char file = 7-(bbChars[0] - 97);
  unsigned char rank = 7-(bbChars[1] - 49);
  
  //cout << "DEBUG: file is " << file << ", rank is " << rank << endl;
  
  //unsigned char index = ((rank) * 8) + (file);
  //unsigned long long bits = 1LLU << index;
  //return bits;
  return bitsFromRankAndFile(rank, file);
}

unsigned long long Bitboard::bitsFromRankAndFile(unsigned char rank, unsigned char file) {
  return indexToBits[(8*(rank))+(file)];
}

unsigned char Bitboard::getRankNumber(unsigned long long bb) {
  unsigned char index = bitsToIndex(bb);
  return (index / 8);
}

unsigned char Bitboard::getFileNumber(unsigned long long bb) {
  unsigned char index = bitsToIndex(bb);
  return (index % 8);
}

void Bitboard::showLegalMoves(std::string move) {
  // deduce piece from bitboard
  unsigned long long bb = stringToSquare(move);
  if (bb & board[p]) {
    // pawn
    board[l] = getPawnMoves(WHITE, bb);
  }
  else if (bb & board[r]) {
    // rook
    board[l] = getRookMoves(WHITE, bb);
  }
  else if (bb & board[n]) {
    // knight
    board[l] = getKnightMoves(WHITE, bb);
  }
  else if (bb & board[b]) {
    // bishop
    board[l] = getBishopMoves(WHITE, bb);
  }
  else if (bb & board[q]) {
    // queen
    board[l] = getQueenMoves(WHITE, bb);
  }
  else if (bb & board[k]) {
    // king
    board[l] = getKingMoves(WHITE, bb);
  }
  else {
    cout << "ERROR: piece on bitboard does not exist" << endl;
  }
  
  // print board
  printBoard();
  
  // remove legal moves
  board[l] = 0LLU;
}

void Bitboard::debugBoard(unsigned long long bb) {
  cout << "   -----------------" << endl;

  unsigned long long i;
  for (i=0; i<64; i++) {
    unsigned long long rank = i/8;
    unsigned long long file = (7 - (i%8));
    unsigned long long square = (1LLU << file) << (8 * rank);
    if (square & bb)
      cout << "1 ";
    else
      cout << "0 ";
    if (((i+1) % 8) == 0)
      cout << endl;
  }
  
  cout << "   -----------------" << endl;
}

void Bitboard::printBoard() {
  cout << "   -----------------" << endl;

  unsigned long long i;
  for (i=0; i<64; i++) {
    if (((i) % 8) == 0) {
      cout << (8 - (i/8)) << " | ";
    }
    
    unsigned long long rank = i/8;
    unsigned long long file = (7 - (i%8));
    unsigned long long square = (1LLU << file) << (8 * rank);
    bool found = false;
    int j;
    for (j=12; j>=0 && !found; j--) {
      if (square & board[j]) {
        found = true;
        cout << pieceNames[j] << " ";
      }
    }
    if (!found)
      cout << "  ";
    if (((i+1) % 8) == 0) {
      cout << "|" << endl;
    } 
  }

  cout << "   -----------------" << endl;
  cout << "    a b c d e f g h" << endl;
}

void Bitboard::processUserMove(string move) {
  // assume opponent is white?
  // TODO fix this
  
  Color opponent = !myColor;
  
  string fromString = move.substr(0, 2);
  unsigned long long from = stringToSquare(fromString);
  
  string toString = move.substr(2, 2);
  unsigned long long to = stringToSquare(toString);
  
  /*
  cout << "DEBUG: moved " << endl;
  debugBoard(from);
  cout << " to " << endl;
  debugBoard(to);
  cout << "/DEBUG" << endl;
  */
  
  // special case: castling
  if (opponent == WHITE) {
    if (move == "e1g1") {
      // white castling
      
      // move king
      togglePiece(k, stringToSquare("e1"), stringToSquare("g1"));
      // move rook
      togglePiece(r, stringToSquare("h1"), stringToSquare("f1"));
      // save move
      if (previousMove != NULL)
        delete previousMove;
      previousMove = new Move(k, stringToSquare("e1"), stringToSquare("g1"));
      previousMove->setCastlingRookFrom(stringToSquare("h1"));
      whiteHasMovedKing = true;
      whiteHasMovedHRook = true;
      return;
    }
    else if (move == "e1c1") {
      togglePiece(k, stringToSquare("e1"), stringToSquare("c1"));
      togglePiece(r, stringToSquare("a1"), stringToSquare("d1"));
      if (previousMove != NULL)
        delete previousMove;
      previousMove = new Move(k, stringToSquare("e1"), stringToSquare("c1"));
      previousMove->setCastlingRookFrom(stringToSquare("a1"));
      whiteHasMovedKing = true;
      whiteHasMovedARook = true;
      return;
    }
  }
  else {
    if (move == "e8g8") {
      // black castling
      
      // move king
      togglePiece(K, stringToSquare("e8"), stringToSquare("g8"));
      // move rook
      togglePiece(R, stringToSquare("h8"), stringToSquare("f8"));
      // save move
      if (previousMove != NULL)
        delete previousMove;
      previousMove = new Move(K, stringToSquare("e8"), stringToSquare("g8"));
      previousMove->setCastlingRookFrom(stringToSquare("h8"));
      blackHasMovedKing = true;
      blackHasMovedHRook = true;
      return;
    }
    else if (move == "e8c8") {
      togglePiece(K, stringToSquare("e8"), stringToSquare("c8"));
      togglePiece(R, stringToSquare("a8"), stringToSquare("d8"));
      if (previousMove != NULL)
        delete previousMove;
      previousMove = new Move(K, stringToSquare("e8"), stringToSquare("c8"));
      previousMove->setCastlingRookFrom(stringToSquare("a8"));
      blackHasMovedKing = true;
      blackHasMovedARook = true;
      return;
    }
  }
  
  // assume it's a regular move
  
  // which piece is it?
  // update the piece on the board
  int offset = 0;
  if (opponent == BLACK) {
    offset = 6;
  }
  
  bool found = false;
  int i;
  for (i=(0+offset); i<(6+offset) && !found; i++) {
    if (board[i] & from) {
      found = true;
      
      /*
      if (i == p || i == P) {
		  // special case: en passant
		  if (previousMove != NULL && previousMove->getPiece() == P) {
			// if previous move was a forward2 move
			
			unsigned long long forward2 = ((((previousMove->getFrom() & rank2) >> 8) & empty()) >> 8) & empty();
			if (previousMove->getTo() & forward2) {
			  // previous move was forward2
			  // try moving it forward 1
			  unsigned long long forward1 = (previousMove->getFrom() >> 8) & empty();
			  if (forward1 & to) {
				// en passant move
				togglePiece(P, previousMove->getTo());
				togglePiece(p, from, to);
				delete previousMove;
				previousMove = new Move(p, from, to);
				return;
			  }
			}
		  }
		  // opponent is white
		  else if (previousMove != NULL && previousMove->getPiece() == p) {
			
			unsigned long long forward2 = ((((previousMove->getFrom() & rank7) << 8) & empty()) << 8) & empty();
			if (previousMove->getTo() & forward2) {
			  // previous move was forward2
			  // try moving it forward 1
			  unsigned long long forward1 = (previousMove->getFrom() << 8) & empty();
			  if (forward1 & to) {
				// en passant move
				togglePiece(p, previousMove->getTo());
				togglePiece(P, from, to);
				delete previousMove;
				previousMove = new Move(P, from, to);
				return;
			  }
			}
		  }
      }
      
      */
      
  	  togglePiece((piece) i, from, to);
  	  // store the move as the previous move
  	  if (previousMove != NULL)
  	    delete previousMove;
      previousMove = new Move(i, from, to);
    }
  }
  
  if (!found) {
    cout << "ERROR (piece not found): usermove does not match existing piece" << endl;
  }
  
  // check for captures
  if (to & black()) {
    // update the board
    for (i=(6-offset); i<(12-offset); i++) {
      if (board[i] & to) {
        togglePiece((piece) i, to);
      }
    }
  }
}

/*
unsigned long long Bitboard::rotate_r90(unsigned long long bb) {
  unsigned long long rotated = 0LLU;
  unsigned long long i;
  for (i=0; i<64; i++) {
    unsigned long long origSquare = (unsigned long long) pow(2, i);
    if (origSquare & bb) {
      unsigned short file = getRankNumber(origSquare);
      unsigned short rank = getFileNumber(origSquare);
      unsigned long long newSquare = file << (rank * 8);
      rotated |= newSquare;
    }
  }
  return rotated;
}
*/

unsigned long long Bitboard::rank_attacks[64][256];
unsigned long long Bitboard::file_attacks[64][256];

void Bitboard::preComputeAttacks() {
  unsigned long long i,j;

  // clear precomputed attack arrays
  for (i=0LLU; i<64LLU; i++) {
    for (j=0LLU; j<256LLU; j++) {
      rank_attacks[i][j] = 0LLU;
      file_attacks[i][j] = 0LLU;
      //diaga1h8_attacks[i][j] = 0LLU;
      //diagh1a8_attacks[i][j] = 0LLU;
    }
  }

  // for each square
  for (i=0LLU; i<64LLU; i++) {
    //cout << "DEBUG: computing square " << i << endl;
    // for each rank_contents
    for (j=0LLU; j<256LLU; j++) {
      //cout << "DEBUG:\t computing occupied squares " << j << endl;
      // compute the rank attacks
      unsigned long long occupied_squares = j << ((i/8LLU) * 8LLU);
      unsigned long long attacks = 0LLU;
      unsigned long long k;
      
      // for files less than current square's file
      for (k=(i%8LLU); k>0LLU; k--) {
        // stop (inclusive) when blocked
        unsigned long long mask = 1LLU << (((i/8LLU) * 8LLU) + (k-1LLU));
        attacks |= mask;
        if (mask & occupied_squares)
          break;
      }
      
      // for files greater than current square's file
      for (k=(i%8LLU)+2LLU; k<=8LLU; k++) {
        // stop (inclusive) when blocked
        unsigned long long mask = 1LLU << (((i/8LLU) * 8LLU) + (k-1LLU));
        attacks |= mask;
        if (mask & occupied_squares)
          break;
      }
      
      rank_attacks[i][j] = attacks;
      
      // for each file_contents
      
      unsigned char file = getFileNumber(indexToBits[i]);
      unsigned char rank = getRankNumber(indexToBits[i]);
      
      // construct occupied_squares bitboard
      occupied_squares = 0LLU;
      attacks = 0LLU;
      for (k=0LLU; k<8LLU; k++) {
        if (j & (1LLU << k)) {
          occupied_squares |= (1LLU << ((k*8LLU) + file));
        }
      }
      
      // go up and down the ranks
      for (k=rank; k>0; k--) {
        unsigned long long mask = 1LLU << (((k-1LLU) * 8LLU) + file);
        attacks |= mask;
        if (mask & occupied_squares)
          break;
      }
      
      for (k=rank+2; k<=8; k++) {
        unsigned long long mask = 1LLU << (((k-1LLU) * 8LLU) + file);
        attacks |= mask;
        if (mask & occupied_squares)
          break;
      }
      
      file_attacks[i][j] = attacks;
      
      /*
      // for each diagonal (a1h8) contents
      
      // construct occupied_squares bitboard
      // find which diagonal we're on
      int diagonal = findDiagonal_a1h8(rank, file);
      
      // "diagonal" tells us which diagonal we're on
      // (8 - abs(diagonal)) is the number of squares on the diagonal
      occupied_squares = 0LLU;
      unsigned char diff = 0;
      unsigned char tempRank = 0;
      unsigned char tempFile = 0;
      if (file > rank) {
        // top left half?
        // going to hit rank 0 first
        diff = rank;
        tempRank = 0;
        tempFile = file - diff;
      }
      else if (file < rank) {
        // bottom right half
        // going to hit file 0 first
        diff = file;
        tempFile = 0;
        tempRank = rank - diff;
      }  
      else {
        // diagonal == 0, center diagonal
        diff = 0;
        tempRank = 0;
        tempFile = 0;
      }
      
      for (k=0LLU; k<(unsigned int) abs(diagonal); k++) {
        if (j & (1LLU << k)) {
          // there's an occupied square at the kth bit
          assert(tempRank <= 7);
          if (tempFile > 7) {
            debugBoard(bitsFromRankAndFile(rank, file));
            printf("DEBUG: tempRank is %u\n", tempRank);
            printf("DEBUG: tempFile is %u\n", tempFile);
            printf("DEBUG: diagonal is %d\n", diagonal);
            printf("DEBUG: abs(diagonal) is %d\n", abs(diagonal));
          }
          assert(tempFile <= 7);
          unsigned long long squareOnDiagonal = bitsFromRankAndFile(tempRank, tempFile);
          occupied_squares |= squareOnDiagonal;
        }
        tempRank++;
        tempFile++;
      }
      
      // go up and down the diagonal
      unsigned char m = rank;
      unsigned char n = file;
      attacks = 0LLU;
      while (m > 0 && n > 0) {
        unsigned char tempRank = m - 1;
        unsigned char tempFile = n - 1;
        assert(tempRank <= 7 && tempFile <= 7);
        unsigned long long squareOnDiagonal = bitsFromRankAndFile(tempRank, tempFile);
        
        attacks |= squareOnDiagonal;
        
        if (squareOnDiagonal & occupied_squares)
          break;
        
        m--;
        n--;
      }
      
      m = rank + 2;
      n = file + 2;
      while (m <= 8 && n <= 8) {
        unsigned char tempRank = m - 1;
        unsigned char tempFile = n - 1;
        assert(tempRank <= 7 && tempFile <= 7);
        unsigned long long squareOnDiagonal = bitsFromRankAndFile(tempRank, tempFile);
        
        attacks |= squareOnDiagonal;
        
        if (squareOnDiagonal & occupied_squares)
          break;
        
        m++;
        n++;
      }
      
      diaga1h8_attacks[i][j] = attacks;
      
      */
    }
  }
}

unsigned char Bitboard::countPieces(unsigned long long bb) {
  unsigned char count = 0;
  
  unsigned char * pieces = (unsigned char *) &bb;
  
  unsigned char i;
  for (i=0; i<8; i++) {
  	// test each byte with the array
  	count += countPiecesConstant[pieces[i]];
  }
  return count;
}

double Bitboard::evaluate() {
  // evaluates the current position on the board[].
  
  // first, sum piece value
  double value = 0.0;
  double total = 0.0;
  int i;
  for (i=0; i<12; i++) {
  	// for each piece type, multiply the value of that piece
  	// times the number of instances
  	unsigned char numPieces = countPieces(board[i]);
  	value += pieceValues[i] * numPieces;
  	total += numPieces;
  }
  
  // weight the piece value sum
  // against the total piece count
  // (being up a piece is worth more
  // when there are fewer pieces total)
  value /= (double) total;
  
  
  // count the number of possible captures for each side
  // add the difference of (white - black)/total to value
  // similar to the "mobility" function
  unsigned long long wMoves = getAllMoves(WHITE);
  unsigned long long wCaptures = wMoves & black();
  //cout << "White captures:" << endl;
  //debugBoard(wCaptures);
  unsigned long long bMoves = getAllMoves(BLACK);
  unsigned long long bCaptures = bMoves & white();
  //cout << "Black captures:" << endl;
  //debugBoard(bCaptures);
  
  int wCapturesNum = countPieces(wCaptures);
  int bCapturesNum = countPieces(bCaptures);
  if (wCapturesNum + bCapturesNum != 0) {
  	double ratio = wCapturesNum - bCapturesNum;
    ratio /= (double) (wCapturesNum + bCapturesNum);
    value += ratio;
  }
  
  
  return value;
}

void Bitboard::zeroOutBoard() {
  // zero out board
  int i;
  for (i=0; i<13; i++) {
  	board[i] = 0LLU;
  	board_r90[i] = 0LLU;
  	//board_ld45[i] = 0LLU;
  	//board_td45[i] = 0LLU;
  }
}

void Bitboard::togglePiece(piece i, unsigned long long from, unsigned long long to) {
  //cout << "---DEBUG---" << endl;
  //debugBoard(board[i]);
  togglePiece(i, from);
  //debugBoard(board[i]);
  togglePiece(i, to);
  //debugBoard(board[i]);
  //cout << "---/DEBUG---" << endl;
}

void Bitboard::togglePiece(piece i, unsigned long long bb) {

  assert(i >= 0);
  assert(i < 12);
  assert(countPieces(bb) == 1);
  // makes the IMPORTANT assumption that bb contains only
  // one instance of one piece!
  // do NOT pass in more than one instance of one piece
  // this sets the piece if it is not set, or unsets it if it is
  // (piece xor)
  board[i] ^= bb;
  board_r90[i] ^= indexToBits[rotate90R[bitsToIndex(bb)]];
  //board_ld45[i] ^= indexToBits[rotate45R[bitsToIndex(bb)]];
  //board_td45[i] ^= indexToBits[rotate45L[bitsToIndex(bb)]];
}

unsigned long long * Bitboard::separatePieces(unsigned long long bb) {
  unsigned long long i;
  unsigned long long * result = new unsigned long long[64];
  for (i=0LLU; i<64LLU; i++) {
    unsigned long long j = indexToBits[i];
    if (bb & j) {
      result[i] = j;
    }
    else {
      result[i] = 0LLU;
    }
  }
  return result;
}

std::string Bitboard::makeBestMove(Color c) {
  //assert(currentGame != NULL);

  //time_t startTime = time(NULL);
  //Move * move = findBestMove(c, this, depth); // deprecated
  
  // step 0: create the thread pool
  
  pthread_t expandThreads[numThreads];
  int expandThreadReturnValues[numThreads];
  
  pthread_t propagateThreads[numThreads];
  int propagateThreadReturnValues[numThreads];
  
  // step 1: generate the tree
  vector<GameTreeNode *> *nodesToExpand = new vector<GameTreeNode *>();
  pthread_mutex_t *nodesToExpandMutex = new pthread_mutex_t;
  pthread_mutex_init(nodesToExpandMutex, NULL);
  
  vector<GameTreeNode *> *nodesToPropagate = new vector<GameTreeNode *>();
  pthread_mutex_t *nodesToPropagateMutex = new pthread_mutex_t;
  pthread_mutex_init(nodesToPropagateMutex, NULL);
  
  Bitboard * originalBitboard = makeCopy(this);
  
  assert(previousMove != NULL);
  GameTreeNode *root = new GameTreeNode(depth, myColor, previousMove->getPiece(), previousMove->getFrom(), previousMove->getTo());
  nodesToExpand->push_back(root);
  
  struct GameTreeNode::expandGameTreeArgs * egta = new struct GameTreeNode::expandGameTreeArgs;
  egta->nodesToExpand = nodesToExpand;
  egta->nodesToExpandMutex = nodesToExpandMutex;
  egta->nodesToPropagate = nodesToPropagate;
  egta->nodesToPropagateMutex = nodesToPropagateMutex;
  egta->originalBitboard = originalBitboard;
  
  struct GameTreeNode::propagateScoresArgs * psa = new struct GameTreeNode::propagateScoresArgs;
  psa->nodesToPropagate = nodesToPropagate;
  psa->nodesToPropagateMutex = nodesToPropagateMutex;
  psa->originalBitboard = originalBitboard;
  
  int i;
  for (i=0; i<numThreads; i++) {
    expandThreadReturnValues[i] = pthread_create(&(expandThreads[i]), NULL, GameTreeNode::expandGameTree, (void*) egta);
    //usleep(200000); // sleep for 0.2 seconds
  }
  
  // wait for the threads to finish
  for (i=0; i<numThreads; i++) {
    pthread_join(expandThreads[i], NULL);
  }
  
  delete egta;
  delete nodesToExpand;
  delete nodesToExpandMutex;
  
  // step 2: propagate scores up the tree
  // (can happen concurrently with step 1
  for (i=0; i<numThreads; i++) {
    propagateThreadReturnValues[i] = pthread_create(&(propagateThreads[i]), NULL, GameTreeNode::propagateScores, (void*) psa);
  }
  
  // wait for the threads to finish
  for (i=0; i<numThreads; i++) {
    pthread_join(propagateThreads[i], NULL);
  }
  
  delete psa;
  delete nodesToPropagate;
  delete nodesToPropagateMutex;
  delete originalBitboard;
  
  // debug game tree
  //GameTreeNode::debugGameTree(root);
  
  // step 3: pick best move
  GameTreeNode *best = GameTreeNode::pickBestMove(root);
  assert(best != NULL);
  
  // save the best move
  Move * move = new Move(best->pieceMoved, best->from, best->to);
  
  // print the game tree for debugging
  GameTreeNode::debugGameTree(root);
  
  // delete the game tree
  delete root;
  
  assert(move != NULL);
  //time_t endTime = time(NULL);
  //cout << "Finding the best move with depth " << depth << " took " << (endTime - startTime) << " seconds." << endl;

  // actually make the move
  
  piece p = (piece) (move->getPiece());
  togglePiece(p, move->getFrom(), move->getTo());
  
  // store the move as previousMove
  if (previousMove != NULL)
    delete previousMove;
  previousMove = move;
  
  string result = move->toString();
  //delete move;
  return result;
}


/*
Move * Bitboard::findBestMove(Color c, Bitboard *currentGame, int localDepth) {
  assert(currentGame != NULL);
  // evaluate every move
  
  //time_t startTime = time(NULL);
  //unsigned int numMoves = 0;
  
  Move *bestMove = NULL;
  
  // TODO use to/from move pairs to speed this up?
  // instead of separatePieces() (slow)
  
  unsigned int i;
  
  unsigned int iStart = 0;
  unsigned int iRange = 6;
  if (c == BLACK)
    iStart += 6;
  
  for (i=iStart; i<(iStart + iRange); i++) {
    //unsigned long long moves = currentGame.getMoves(c, (piece) i);
    unsigned long long * from = separatePieces(currentGame->board[i]);
    
    unsigned int j;
    for (j=0; j<64; j++) {
      if (from[j] == 0LLU)
        continue;
      
      // from[j] has a piece in it
    
      unsigned long long moves = currentGame->getSinglePieceMoves(c, (piece) i, from[j]);
      
      if (moves == 0LLU) {
        //cout << "DEBUG: piece " << pieceNames[i] << " has no moves." << endl;
        continue;
      }
      
      unsigned long long * to = separatePieces(moves);
      
      // make each move, test how good it is
      unsigned int k;
      for (k=0; k<64; k++) {
        if (to[k] == 0LLU)
          continue;
        
        // to[k] has a move in it
        //numMoves++;
        
        Move move(i, from[j], to[k]);
        Bitboard * nextMove = new Bitboard(*currentGame, move);
        
        if (localDepth == 0) {
          // check detection
          if (nextMove->inCheck(c)) {
            // we do not want to be in check next move!!
            // give it the worst score possible
            if (c == WHITE)
              move.setScore(DBL_MIN);
            else
              move.setScore(DBL_MAX);
          }
          else
            move.setScore(nextMove->evaluate());
        }
        
        else {
          Move * bestResult = findBestMove(!c, nextMove, localDepth - 1);
          move.setScore(bestResult->getScore());
          delete bestResult;
          
          if (localDepth == depth) {
            cout << "DEBUG: evaluated " << pieceNames[i] << " " << move.toString() << " as " << move.getScore() << endl;
          }
        }
        
        delete nextMove;
        
        if (c == WHITE) {
          if (bestMove == NULL || move.getScore() > bestMove->getScore()) {
            delete bestMove;
            bestMove = new Move(move);
          }
        }
        else { // c is BLACK
          if (bestMove == NULL || move.getScore() < bestMove->getScore()) {
            delete bestMove;
            bestMove = new Move(move);
          }
        }
        
      }
      
      delete[] to;
      
    }
    
    delete[] from;
    
  }
  
  //time_t endTime = time(NULL);
  
  //cout << "Evaluating " << numMoves << " moves at depth " << depth << " took " << (endTime - startTime) << " seconds." << endl;
  
  assert(bestMove != NULL);
  
  return bestMove;
}
*/

/*
int Bitboard::findDiagonal_a1h8(unsigned char rank, unsigned char file) {
  int diagonal = (7 - file) - rank;
  assert(diagonal >= -7 && diagonal <= 7);
  
  return diagonal;
}

int Bitboard::findDiagonal_h1a8(unsigned char rank, unsigned char file) {
  int diagonal = file - rank;
  assert(diagonal >= -7 && diagonal <= 7);
  
  return diagonal;
}
*/

bool Bitboard::inCheck(Color c) {
  unsigned long long enemyMoves = getAllMoves(!c);
  if (c == BLACK) {
    if (enemyMoves & board[K]) {
      //cout << "DEBUG: in check!" << endl;
      return true;
    }
  }
  else {
    if (enemyMoves & board[k]) {
      //cout << "DEBUG: in check!" << endl;
      return true;
    }
  }
  return false;
}

bool Bitboard::canCastle(Color c) {
  if (!inCheck(c)) {
    if (c == WHITE) {
      if (!whiteHasMovedKing && (!whiteHasMovedARook || !whiteHasMovedHRook))
        return true;
    }
    else {
      if (!blackHasMovedKing && (!blackHasMovedARook || !blackHasMovedHRook))
        return true;
    }
  }
  return false;
}

Bitboard * Bitboard::makeCopy(const Bitboard * original) {
  Bitboard * copy = new Bitboard(original->myColor);
  unsigned char i;
  for (i=0; i<12; i++) {
    copy->board[i] = original->board[i];
    copy->board_r90[i] = original->board_r90[i];
    //copy->board_ld45[i] = original->board_ld45[i];
    //copy->board_td45[i] = original->board_td45[i];
  }
  
  copy->whiteHasMovedKing = original->whiteHasMovedKing;
  copy->blackHasMovedKing = original->blackHasMovedKing;
  copy->whiteHasMovedARook = original->whiteHasMovedARook;
  copy->blackHasMovedARook = original->blackHasMovedARook;
  copy->whiteHasMovedHRook = original->whiteHasMovedHRook;
  copy->blackHasMovedHRook = original->blackHasMovedHRook;
  return copy;
}
