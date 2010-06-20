#ifndef _BITBOARD_H_
#define _BITBOARD_H_

#include <map>
#include "Move.h"

#define WHITE true
#define BLACK false
#define Color bool

// bitboard masks
#define rank8 0x00000000000000FFLLU
#define rank7 0x000000000000FF00LLU
#define rank6 0x0000000000FF0000LLU
#define rank5 0x00000000FF000000LLU
#define rank4 0x000000FF00000000LLU
#define rank3 0x0000FF0000000000LLU
#define rank2 0x00FF000000000000LLU
#define rank1 0xFF00000000000000LLU
#define rank12 0xFFFF000000000000LLU
#define rank78 0x000000000000FFFFLLU
#define filea 0x8080808080808080LLU
#define fileb 0x4040404040404040LLU
#define filec 0x2020202020202020LLU
#define filed 0x1010101010101010LLU
#define filee 0x0808080808080808LLU
#define filef 0x0404040404040404LLU
#define fileg 0x0202020202020202LLU
#define fileh 0x0101010101010101LLU
#define fileab 0xC0C0C0C0C0C0C0C0LLU
#define filegh 0x0303030303030303LLU

#define DEBUG_FULLBOARD 0xFFFFFFFFFFFFFFFFLLU

class Bitboard {
	private:
		// precomputed attacks
		// TODO: optimize this to 6 bits instead of 8 for occupied
		static unsigned long long rank_attacks[64][256];
		static unsigned long long file_attacks[64][256];
		//unsigned long long diaga1h8_attacks[64][256];
		//unsigned long long diagh1a8_attacks[64][256];
		
		void setupBoard();
		unsigned long long white();
		unsigned long long black();
		unsigned long long occupied();
		unsigned long long occupied_r90();
		//unsigned long long occupied_ld45();
		//unsigned long long occupied_td45();
		unsigned long long empty();
		
		static unsigned long long bitsFromRankAndFile(unsigned char, unsigned char);
		static unsigned char getRankNumber(unsigned long long);
		static unsigned char getFileNumber(unsigned long long);
		static unsigned char bitsToIndex(unsigned long long);
		//static unsigned long long indexToBits(unsigned short);
		
		
		unsigned long long getAllMoves(Color);
		unsigned long long getPawnMoves(Color, unsigned long long);
		unsigned long long getRookMoves(Color, unsigned long long);
		unsigned long long getKnightMoves(Color, unsigned long long);
		unsigned long long getBishopMoves(Color, unsigned long long);
		unsigned long long getQueenMoves(Color, unsigned long long);
		unsigned long long getKingMoves(Color, unsigned long long);
		unsigned long long getCastlingMoves(Color); // shows king moves
		
		unsigned long long pickRandomOccupiedSquare(unsigned long long);
		//unsigned long long rotate_r90(unsigned long long);
		
		
		static unsigned char countPieces(unsigned long long);
		
		void zeroOutBoard();
		
		void testEvaluate();
		Bitboard(Bitboard old, Move move);
		
		//static int findDiagonal_a1h8(unsigned char rank, unsigned char file);
		//static int findDiagonal_h1a8(unsigned char rank, unsigned char file);
		
		bool inCheck(Color);
		
		bool canCastle(Color c);
		bool whiteHasMovedKing;
		bool blackHasMovedKing;
		bool whiteHasMovedARook;
		bool blackHasMovedARook;
		bool whiteHasMovedHRook;
		bool blackHasMovedHRook;
		
		

	public:
	
		Color myColor;
	    Move *previousMove;
	
		enum piece {p, k, q, n, b, r, P, K, Q, N, B, R, l};
	
	    static int depth;
	    static int numThreads;
	    
	    unsigned long long board[13];
		unsigned long long board_r90[13];
		//unsigned long long board_ld45[13];
		//unsigned long long board_td45[13];
	    
	    ~Bitboard();
		Bitboard(Color);
		Bitboard(Bitboard *old, unsigned int xyz, unsigned long long from, unsigned long long to);
		
		std::string makeRandomMove(Color);
		void showLegalMoves(std::string);
		void printBoard();
		static void debugBoard(unsigned long long);
		void processUserMove(std::string);
		double evaluate();
		unsigned long long getMoves(Color, piece);
		unsigned long long getSinglePieceMoves(Color, piece, unsigned long long);
		unsigned long long getSinglePieceMoves(Color, unsigned int, unsigned long long);
		static unsigned long long * separatePieces(unsigned long long);
		std::string makeBestMove(Color);
		static Move * findBestMove(Color, Bitboard*, int);
		static std::string squareToString(unsigned long long);
		static unsigned long long stringToSquare(std::string);
		
		static Bitboard * makeCopy(const Bitboard * original);
		
		static void preComputeAttacks();
		
		// piece, from, to
		void togglePiece(piece, unsigned long long, unsigned long long);
		
		// run twice (once piece, from, once piece, to)
		void togglePiece(piece, unsigned long long);
};



#endif
