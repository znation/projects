#include "Bitboard.h"

unsigned char Bitboard::bitsToIndex(unsigned long long bb) {
  switch (bb) {
    case 0x0000000000000001LLU: return 0;
    case 0x0000000000000002LLU: return 1;
    case 0x0000000000000004LLU: return 2;
    case 0x0000000000000008LLU: return 3;
    case 0x0000000000000010LLU: return 4;
    case 0x0000000000000020LLU: return 5;
    case 0x0000000000000040LLU: return 6;
    case 0x0000000000000080LLU: return 7;
    case 0x0000000000000100LLU: return 8;
    case 0x0000000000000200LLU: return 9;
    case 0x0000000000000400LLU: return 10;
    case 0x0000000000000800LLU: return 11;
    case 0x0000000000001000LLU: return 12;
    case 0x0000000000002000LLU: return 13;
    case 0x0000000000004000LLU: return 14;
    case 0x0000000000008000LLU: return 15;
    case 0x0000000000010000LLU: return 16;
    case 0x0000000000020000LLU: return 17;
    case 0x0000000000040000LLU: return 18;
    case 0x0000000000080000LLU: return 19;
    case 0x0000000000100000LLU: return 20;
    case 0x0000000000200000LLU: return 21;
    case 0x0000000000400000LLU: return 22;
    case 0x0000000000800000LLU: return 23;
    case 0x0000000001000000LLU: return 24;
    case 0x0000000002000000LLU: return 25;
    case 0x0000000004000000LLU: return 26;
    case 0x0000000008000000LLU: return 27;
    case 0x0000000010000000LLU: return 28;
    case 0x0000000020000000LLU: return 29;
    case 0x0000000040000000LLU: return 30;
    case 0x0000000080000000LLU: return 31;
    case 0x0000000100000000LLU: return 32;
    case 0x0000000200000000LLU: return 33;
    case 0x0000000400000000LLU: return 34;
    case 0x0000000800000000LLU: return 35;
    case 0x0000001000000000LLU: return 36;
    case 0x0000002000000000LLU: return 37;
    case 0x0000004000000000LLU: return 38;
    case 0x0000008000000000LLU: return 39;
    case 0x0000010000000000LLU: return 40;
    case 0x0000020000000000LLU: return 41;
    case 0x0000040000000000LLU: return 42;
    case 0x0000080000000000LLU: return 43;
    case 0x0000100000000000LLU: return 44;
    case 0x0000200000000000LLU: return 45;
    case 0x0000400000000000LLU: return 46;
    case 0x0000800000000000LLU: return 47;
    case 0x0001000000000000LLU: return 48;
    case 0x0002000000000000LLU: return 49;
    case 0x0004000000000000LLU: return 50;
    case 0x0008000000000000LLU: return 51;
    case 0x0010000000000000LLU: return 52;
    case 0x0020000000000000LLU: return 53;
    case 0x0040000000000000LLU: return 54;
    case 0x0080000000000000LLU: return 55;
    case 0x0100000000000000LLU: return 56;
    case 0x0200000000000000LLU: return 57;
    case 0x0400000000000000LLU: return 58;
    case 0x0800000000000000LLU: return 59;
    case 0x1000000000000000LLU: return 60;
    case 0x2000000000000000LLU: return 61;
    case 0x4000000000000000LLU: return 62;
    case 0x8000000000000000LLU: return 63;
  }
  return 0; // shouldn't get here
}
