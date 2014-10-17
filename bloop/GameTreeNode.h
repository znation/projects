#ifndef _GAMETREENODE_H_
#define _GAMETREENODE_H_

#include <vector>
#include <string>
#include "Bitboard.h"
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

class GameTreeNode {
  private:
    static void debugGameTreeRecursive(GameTreeNode * root);
    
  public:
    //Bitboard * bitboard;
    int depth;
    Color toMove;
    unsigned char pieceMoved;
    unsigned long long from, to;
    double moveScore;
    bool hasMoveScore;
    int numChildren;
    GameTreeNode * parent;
    
    pthread_mutex_t mutex;
    
    
    std::vector<GameTreeNode *> children;
    
    GameTreeNode(int d, Color c, unsigned char p, unsigned long long f, unsigned long long t); // constructor
    ~GameTreeNode(); // destructor
    
    // starts with the root of the tree in nodesToExpand
    static void * expandGameTree(void *);
    struct expandGameTreeArgs {
      std::vector<GameTreeNode *> *nodesToExpand;
      std::vector<GameTreeNode *> *nodesToPropagate;
      pthread_mutex_t *nodesToExpandMutex;
      pthread_mutex_t *nodesToPropagateMutex;
      Bitboard *originalBitboard;
    };
    
    // starts with the leaves of the tree in nodesToPropagate
    static void * propagateScores(void *);
    struct propagateScoresArgs {
      std::vector<GameTreeNode *> *nodesToPropagate;
      pthread_mutex_t *nodesToPropagateMutex;
      Bitboard *originalBitboard;
    };
    
    // starts with the root of the tree passed in
    static GameTreeNode * pickBestMove(GameTreeNode * root);
    
    std::string toString();

	static void debugGameTree(GameTreeNode * root);
};

#endif
