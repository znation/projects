
static unsigned long long diagonalShiftAmount[15] = {
  0LLU,		// 1
  2LLU,		// 2
  5LLU,		// 3
  9LLU,	    // 4
  14LLU,	// 5
  20LLU,	// 6
  27LLU,	// 7
  35LLU,	// 8
  42LLU,	// 7
  48LLU,	// 6
  53LLU,	// 5
  57LLU,	// 4
  60LLU,	// 3
  62LLU,	// 2
  63LLU	    // 1
};

static unsigned char diagonalShiftMask[15] = {
  1,
  3,
  7,
  15,
  31,
  63,
  127,
  255,
  127,
  63,
  31,
  15,
  7,
  3,
  1
};

static unsigned char diagonalPieceCount[15] = {
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  8,
  7, // 9
  6, // 10
  5, // 11
  4, // 12
  3, // 13
  2, // 14
  1  // 15
};

static int rotate90L[64] = {
  7, 15, 23, 31, 39, 47, 55, 63, 
  6, 14, 22, 30, 38, 46, 54, 62, 
  5, 13, 21, 29, 37, 45, 53, 61, 
  4, 12, 20, 28, 36, 44, 52, 60, 
  3, 11, 19, 27, 35, 43, 51, 59, 
  2, 10, 18, 26, 34, 42, 50, 58, 
  1, 9, 17, 25, 33, 41, 49, 57, 
  0, 8, 16, 24, 32, 40, 48, 56
};

