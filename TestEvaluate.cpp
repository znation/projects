#include <iostream>
#include "Bitboard.h"

using namespace std;

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

void Bitboard::testEvaluate() {
  // test out evaluation function
  
  // levon board position 1:
  zeroOutBoard();
  board[k] = stringToSquare("e1");
  board[q] = stringToSquare("d1");
  board[K] = stringToSquare("e8");
  
  // show board and evaluation for position 1
  printBoard();
  cout << endl << "Position 1" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
  
  // levon board position 2:
  zeroOutBoard();
  board[r] = stringToSquare("a1") | stringToSquare("h1");
  board[n] = stringToSquare("b1") | stringToSquare("g1");
  board[b] = stringToSquare("c1") | stringToSquare("f1");
  board[q] = stringToSquare("h7");
  board[k] = stringToSquare("e1");
  board[p] = stringToSquare("a2") | stringToSquare("b2") | 
    stringToSquare("c2") | stringToSquare("d2") |
    stringToSquare("e4") | stringToSquare("f2") |
    stringToSquare("g2") | stringToSquare("h2");
  board[R] = stringToSquare("a8");
  board[N] = stringToSquare("b8");
  board[B] = stringToSquare("c8") | stringToSquare("f8");
  board[Q] = stringToSquare("d8");
  board[K] = stringToSquare("e8");
  board[P] = stringToSquare("a7") | stringToSquare("b7") |
    stringToSquare("c7") | stringToSquare("d6") |
    stringToSquare("f7") | stringToSquare("g6");
   
  // show board and evaluation for position 2
  printBoard();
  cout << endl << "Position 2" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
  
  // levon board position 3:
  zeroOutBoard();
  board[k] = stringToSquare("e1");
  board[p] = stringToSquare("h2");
  board[K] = stringToSquare("e8");
  board[R] = stringToSquare("g8");
  
  // show board and evaluation for position 3
  printBoard();
  cout << endl << "Position 3" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
  
  // levon board position 4:
  zeroOutBoard();
  board[r] = startingBoard[r];
  board[n] = startingBoard[n];
  board[b] = stringToSquare("c1") | stringToSquare("c4");
  board[q] = stringToSquare("b4");
  board[k] = startingBoard[k];
  board[p] = startingBoard[p] ^ (stringToSquare("c2") | stringToSquare("e2") | stringToSquare("d5"));
  board[R] = startingBoard[R];
  board[N] = stringToSquare("c2") | stringToSquare("f6");
  board[B] = stringToSquare("c8");
  board[Q] = startingBoard[Q];
  board[K] = startingBoard[K];
  board[P] = startingBoard[P] ^ (stringToSquare("d7") | stringToSquare("e7") |
    stringToSquare("g7") | stringToSquare("e5") | stringToSquare("g6"));
  
  // show board and evaluation for position 4
  printBoard();
  cout << endl << "Position 4" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
  
  // levon board position 5:
  zeroOutBoard();
  board[r] = stringToSquare("f4");
  board[k] = stringToSquare("g2");
  board[p] = stringToSquare("g3") | stringToSquare("h4");
  board[N] = stringToSquare("f7");
  board[K] = stringToSquare("g7");
  board[P] = stringToSquare("g6") | stringToSquare("h7");
  
  // show board and evaluation for position 5
  printBoard();
  cout << endl << "Position 5" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
  
  // levon board position 6:
  zeroOutBoard();
  board[r] = stringToSquare("a1") | stringToSquare("h3");
  board[n] = stringToSquare("d2") | stringToSquare("e5");
  board[b] = stringToSquare("d3") | stringToSquare("g5");
  board[q] = stringToSquare("h4");
  board[k] = stringToSquare("g1");
  board[p] = startingBoard[p] ^ (stringToSquare("d2") |
    stringToSquare("d4") | stringToSquare("e2") |
    stringToSquare("e3") | stringToSquare("f2") |
    stringToSquare("f4"));
  board[R] = stringToSquare("a8") | stringToSquare("f8");
  board[N] = stringToSquare("b5") | stringToSquare("f6");
  board[B] = stringToSquare("e8") | stringToSquare("e7");
  board[Q] = stringToSquare("d8");
  board[K] = stringToSquare("g8");
  board[P] = startingBoard[P] ^ (stringToSquare("c7") |
    stringToSquare("c6") | stringToSquare("d7") |
    stringToSquare("d5") | stringToSquare("e7") |
    stringToSquare("e6") | stringToSquare("g7") |
    stringToSquare("g6"));
  
  // show board and evaluation for position 6
  printBoard();
  cout << endl << "Position 6" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
  
  // levon board position 7:
  zeroOutBoard();
  board[p] = stringToSquare("c6") | stringToSquare("e6") |
    stringToSquare("g5") | stringToSquare("h4");
  board[k] = stringToSquare("d4");
  board[b] = stringToSquare("e5") | stringToSquare("e4");
  board[K] = stringToSquare("b8");
  board[B] = stringToSquare("e8");
  board[N] = stringToSquare("g8");
  board[P] = stringToSquare("c7") | stringToSquare("e7") |
    stringToSquare("g6") | stringToSquare("h7");
  
  // show board and evaluation for position 7
  printBoard();
  cout << endl << "Position 7" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
  
  // levon board position 8:
  zeroOutBoard();
  board[n] = stringToSquare("c3") | stringToSquare("e2");
  board[k] = stringToSquare("e1");
  board[R] = stringToSquare("e7");
  board[B] = stringToSquare("f6");
  board[K] = stringToSquare("g5");
  
  // show board and evaluation for position 8
  printBoard();
  cout << endl << "Position 8" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
  
  // levon board position 9:
  zeroOutBoard();
  board[r] = stringToSquare("a1") | stringToSquare("e1");
  board[n] = stringToSquare("c3") | stringToSquare("f3");
  board[b] = stringToSquare("d2") | stringToSquare("f1");
  board[q] = stringToSquare("c1");
  board[k] = stringToSquare("g1");
  board[p] = startingBoard[p] ^ (stringToSquare("a2") | stringToSquare("a3") |
    stringToSquare("c2") | stringToSquare("d2") | stringToSquare("d4") |
    stringToSquare("e2") | stringToSquare("e3"));
  board[R] = startingBoard[R];
  board[N] = stringToSquare("d7") | stringToSquare("f6");
  board[B] = stringToSquare("b7") | stringToSquare("d6");
  board[Q] = stringToSquare("c7");
  board[K] = stringToSquare("e8");
  board[P] = stringToSquare("a6") | stringToSquare("b5") |
    stringToSquare("c5") | stringToSquare("c4") |
    stringToSquare("e6") | stringToSquare("f7") |
    stringToSquare("g7") | stringToSquare("h7");
  
  // show board and evaluation for position 9
  printBoard();
  cout << endl << "Position 9" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
  
  // levon board position 10:
  zeroOutBoard();
  board[p] = stringToSquare("a4") | stringToSquare("b3") |
    stringToSquare("c4") | stringToSquare("f3") |
    stringToSquare("g4");
  board[b] = stringToSquare("c3") | stringToSquare("e4");
  board[k] = stringToSquare("e3");
  board[N] = stringToSquare("b8");
  board[B] = stringToSquare("c7");
  board[K] = stringToSquare("f7");
  board[P] = stringToSquare("a5") | stringToSquare("b6") |
    stringToSquare("c5") | stringToSquare("f6") | stringToSquare("g7");
  
  // show board and evaluation for position 10
  printBoard();
  cout << endl << "Position 10" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
  
  // levon board position 11:
  zeroOutBoard();
  board[p] = startingBoard[p] ^ (stringToSquare("d2") |
    stringToSquare("d3") | stringToSquare("e2") | stringToSquare("e4") |
    stringToSquare("f2") | stringToSquare("f4"));
  board[n] = stringToSquare("c3") | stringToSquare("f3");
  board[b] = stringToSquare("c1") | stringToSquare("c4");
  board[q] = stringToSquare("g3");
  board[k] = stringToSquare("g1");
  board[r] = stringToSquare("a1") | stringToSquare("f1");
  board[P] = startingBoard[P] ^ (stringToSquare("b7") |
    stringToSquare("b6") | stringToSquare("d7") | stringToSquare("d6") |
    stringToSquare("e7") | stringToSquare("e5"));
  board[R] = stringToSquare("a8") | stringToSquare("f8");
  board[B] = stringToSquare("b7") | stringToSquare("e7");
  board[N] = stringToSquare("d7") | stringToSquare("f6");
  board[Q] = stringToSquare("d8");
  board[K] = stringToSquare("g8");
  
  // show board and evaluation for position 11
  printBoard();
  cout << endl << "Position 11" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
  
  // levon board position 12:
  zeroOutBoard();
  board[p] = stringToSquare("a2") | stringToSquare("e4") |
    stringToSquare("f3") | stringToSquare("h3");
  board[r] = stringToSquare("f2");
  board[k] = stringToSquare("g3");
  board[P] = stringToSquare("a6") | stringToSquare("f6") |
    stringToSquare("g5") | stringToSquare("h6");
  board[R] = stringToSquare("a3");
  board[K] = stringToSquare("e5");
  
  // show board and evaluation for position 12
  printBoard();
  cout << endl << "Position 12" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
  
  // levon board position 13:
  zeroOutBoard();
  board[p] = startingBoard[p] ^ (stringToSquare("d2") |
    stringToSquare("e2") | stringToSquare("f2") | stringToSquare("f3"));
  board[r] = startingBoard[r];
  board[b] = startingBoard[b];
  board[n] = stringToSquare("c3") | stringToSquare("e2");
  board[q] = startingBoard[q];
  board[k] = startingBoard[k];
  board[P] = startingBoard[P] ^ (stringToSquare("c7") |
    stringToSquare("d7") | stringToSquare("e7") | stringToSquare("e6"));
  board[R] = startingBoard[R];
  board[B] = stringToSquare("b4") | stringToSquare("c8");
  board[N] = stringToSquare("c6") | stringToSquare("d5");
  board[Q] = startingBoard[Q];
  board[K] = startingBoard[K];
  
  // show board and evaluation for position 13
  printBoard();
  cout << endl << "Position 13" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
  
  // levon board position 14:
  zeroOutBoard();
  board[p] = stringToSquare("a2") | stringToSquare("b2") |
    stringToSquare("c3") | stringToSquare("d4") | stringToSquare("f2") |
    stringToSquare("g2") | stringToSquare("h3");
  board[r] = stringToSquare("e1") | stringToSquare("f1");
  board[n] = stringToSquare("d2") | stringToSquare("f3");
  board[q] = stringToSquare("d3");
  board[k] = stringToSquare("g1");
  board[P] = startingBoard[P] ^ (stringToSquare("c7") |
    stringToSquare("c6") | stringToSquare("d7") |
    stringToSquare("d5") | stringToSquare("e7"));
  board[R] = stringToSquare("e8") | stringToSquare("f8");
  board[N] = stringToSquare("d7") | stringToSquare("f6");
  board[Q] = stringToSquare("d6");
  board[K] = stringToSquare("g1");
  
  // show board and evaluation for position 14
  printBoard();
  cout << endl << "Position 14" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
  
  // levon board position 15:
  zeroOutBoard();
  board[r] = stringToSquare("a8");
  board[k] = stringToSquare("g6");
  board[p] = stringToSquare("h6");
  board[P] = stringToSquare("a3");
  board[K] = stringToSquare("b2");
  board[R] = stringToSquare("b1");
  
  // show board and evaluation for position 15
  printBoard();
  cout << endl << "Position 15" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
  
  // levon board position 16:
  zeroOutBoard();
  board[p] = stringToSquare("a2") | stringToSquare("b2") |
    stringToSquare("c2") | stringToSquare("g4");
  board[r] = stringToSquare("d1");
  board[n] = stringToSquare("b1") | stringToSquare("d4");
  board[b] = stringToSquare("g5") | stringToSquare("h1");
  board[q] = stringToSquare("d2");
  board[k] = stringToSquare("c1");
  board[P] = stringToSquare("a6") | stringToSquare("b4");
  board[R] = stringToSquare("c8");
  board[N] = stringToSquare("c5") | stringToSquare("d5");
  board[B] = stringToSquare("b7") | stringToSquare("g7");
  board[Q] = stringToSquare("a5");
  board[K] = stringToSquare("f7");
  
  // show board and evaluation for position 16
  printBoard();
  cout << endl << "Position 16" << endl;
  cout << "Evaluation is: " << evaluate() << endl << endl;
}
