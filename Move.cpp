#include "Move.h"
#include <string>
#include <assert.h>
#include <sstream>
#include "Bitboard.h"

using namespace std;

static unsigned long long a1 = 0x8000000000000000LLU;
static unsigned long long a8 = 0x0000000000000080LLU;
static unsigned long long h1 = 0x0100000000000000LLU;
static unsigned long long h8 = 0x0000000000000001LLU;
static unsigned long long d1 = 0x1000000000000000LLU;
static unsigned long long d8 = 0x0000000000000010LLU;
static unsigned long long f1 = 0x0400000000000000LLU;
static unsigned long long f8 = 0x0000000000000004LLU;

Move::Move() {
  pieceNum = 13;
  to = 0LLU;
  from = 0LLU;
  score = 0;
  isCastling = false;
  castlingRookFrom = 0LLU;
}

Move::Move(unsigned int xyz, unsigned long long f, unsigned long long t) {
  setMoveData(xyz, f, t, 0);
  isCastling = false;
  castlingRookFrom = 0LLU;
}

void Move::setMoveData(unsigned int xyz, unsigned long long f, unsigned long long t, double s) {
  pieceNum = xyz;
  
  assert(pieceNum >= 0);
  assert(pieceNum < 12);
  
  score = s;
  
  from = f;
  to = t;
}

std::string Move::toString() {
  
  stringstream ss;
  ss << Bitboard::squareToString(from);
  ss << Bitboard::squareToString(to);
  
  string out;
  ss >> out;
  return out;
}

unsigned int Move::getPiece() {
  return pieceNum;
}

unsigned long long Move::getFrom() {
  return from;
}

unsigned long long Move::getTo() {
  return to;
}

void Move::setScore(double s) {
  // lower scores are better for black
  score = s;
}

double Move::getScore() {
  return score;
}

bool Move::getCastling() {
  return isCastling;
}

// don't call this unless you know the move is a castling move
unsigned long long Move::castlingRook() {
  assert(pieceNum == k || pieceNum == K);
  if (pieceNum == k)
    return r;
  else if (pieceNum == K)
    return R;
  
  return l; // in this context, l means error/not castling
}

void Move::setCastlingRookFrom(unsigned long long rook) {
  castlingRookFrom = rook;
  isCastling = true;
}

unsigned long long Move::getCastlingRookFrom() {
  return castlingRookFrom;
}

unsigned long long Move::getCastlingRookTo() {
  assert(pieceNum == k || pieceNum == K);
  if (pieceNum == k) {
    // white rook
    assert(castlingRookFrom == a1 || castlingRookFrom == h1);
    if (castlingRookFrom == a1) {
      return d1;
    }
    else if (castlingRookFrom == h1) {
      return f1;
    }
  }
  
  else if (pieceNum == K) {
    // black rook
    assert(castlingRookFrom == a8 || castlingRookFrom == h8);
    if (castlingRookFrom == a8) {
      return d8;
    }
    else if (castlingRookFrom == h8) {
      return f8;
    }
  }
  
  return 0LLU;
}
