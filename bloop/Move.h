#ifndef _MOVE_H_
#define _MOVE_H_

#include <string>

class Move {
  private:
    unsigned int pieceNum;
    unsigned long long from;
    unsigned long long to;
    double score;
    bool isCastling;
    unsigned long long castlingRookFrom;
    // setMoveData(piece, bitboard from, bitboard to, double score)
    void setMoveData(unsigned int xyz, unsigned long long f, unsigned long long t, double s);
    enum piece {p, k, q, n, b, r, P, K, Q, N, B, R, l};
    
  public:
    Move();
    // Move(piece, bitboard from, bitboard to)
    Move(unsigned int, unsigned long long, unsigned long long);
    std::string toString();
    void setScore(double);
    double getScore();
    unsigned long long getFrom();
    unsigned long long getTo();
    unsigned int getPiece();
    bool getCastling();
    unsigned long long castlingRook();
    void setCastlingRookFrom(unsigned long long);
    unsigned long long getCastlingRookFrom();
    unsigned long long getCastlingRookTo();

};

#endif
