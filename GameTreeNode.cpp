#include "GameTreeNode.h"
#include <sstream>
#include <iostream>
#include <assert.h>
#include <fstream>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

using namespace std;

GameTreeNode::GameTreeNode(int d, Color c, unsigned char p,
  unsigned long long f, unsigned long long t)
  :
  //bitboard(b),
  depth(d),
  toMove(c),
  pieceMoved(p),
  from(f),
  to(t),
  moveScore(0.0),
  hasMoveScore(false),
  numChildren(0),
  parent(NULL)
{
  /*
  bitboard = b;
  depth = d;
  toMove = c;
  pieceMoved = p;
  from = f;
  to = t;
  moveScore = 0.0;
  hasMoveScore = false;
  numChildren = 0;
  parent = NULL;
  */
  pthread_mutex_init(&mutex, NULL);
}

GameTreeNode::~GameTreeNode() {
  // delete children
  while (children.size() > 0) {
    GameTreeNode *child = children.back();
    children.pop_back();
    
    delete child;
  }
  
  // delete own items
  //delete bitboard;
}

GameTreeNode * GameTreeNode::pickBestMove(GameTreeNode * root) {
  GameTreeNode * bestMove = NULL;
  
  unsigned int i;
  for (i=0; i<root->children.size(); i++) {
    GameTreeNode * child = root->children.at(i);
    
    assert(child->hasMoveScore);
    
    if (bestMove == NULL) {
      bestMove = child;
    }
    else {
      if (root->toMove == WHITE) {
        // higher scores are better
        if (child->moveScore > bestMove->moveScore)
          bestMove = child;
      }
      else {
        // lower scores are better
        if (child->moveScore < bestMove->moveScore)
          bestMove = child;
      }
    }
  }
  
  assert(bestMove != NULL);
  return bestMove;
}

// starts with the leaves (bottom) of tree
void * GameTreeNode::propagateScores(void * args) {
  cerr << "Thread " << pthread_self() << " in propagateScores() (line 86)" << endl;
  struct propagateScoresArgs * psa = (struct propagateScoresArgs *) args;
  
  assert(psa != NULL);
  
  vector<GameTreeNode *> *nodesToPropagate = psa->nodesToPropagate;
  pthread_mutex_t *nodesToPropagateMutex = psa->nodesToPropagateMutex;
  Bitboard *originalBitboard = psa->originalBitboard;
  
  assert(nodesToPropagate != NULL);
  assert(nodesToPropagateMutex != NULL);
  assert(originalBitboard != NULL);

  cerr << "Thread " << pthread_self() << " locking nodesToPropagate (line 99)" << endl;
  pthread_mutex_lock(nodesToPropagateMutex);
  unsigned int size = nodesToPropagate->size();
  cerr << "Thread " << pthread_self() << " unlocking nodesToPropagate (line 102)" << endl;
  pthread_mutex_unlock(nodesToPropagateMutex);
  
  while (size > 0) {
    //printf("Thread %u in propagateScores() while loop\n", (unsigned int) pthread_self());
    // pop a node off the stack
    
    GameTreeNode * current = NULL;
    
    cerr << "Thread " << pthread_self() << " locking nodesToPropagate (line 111)" << endl;
    pthread_mutex_lock(nodesToPropagateMutex);
    if (nodesToPropagate->size() > 0) {
      //current = nodesToPropagate->back();
      //nodesToPropagate->pop_back();
      
      // pop from the front
      current = nodesToPropagate->at(0);
      nodesToPropagate->erase(nodesToPropagate->begin());
      
      cerr << "Thread " << pthread_self() << " unlocking nodesToPropagate (line 121)" << endl;
      pthread_mutex_unlock(nodesToPropagateMutex);
      
    }
    else {
      cerr << "Thread " << pthread_self() << " unlocking nodesToPropagate (line 126)" << endl;
      pthread_mutex_unlock(nodesToPropagateMutex);
      // size was 0 by the time we got to this part. we die now.
      break;
    }
    
    assert(current != NULL);
    
    cerr << "Thread " << pthread_self() << " locking move " << current << " (line 134)" << endl;
    pthread_mutex_lock(&current->mutex);
    
    if (current->parent == NULL) {
      // hit the root of the tree
      cerr << "Thread " << pthread_self() << " unlocking move " << current << " (line 139)" << endl;
      pthread_mutex_unlock(&current->mutex);
      
      // update the loop variable
      cerr << "Thread " << pthread_self() << " locking nodesToPropagate (line 143)" << endl;
      pthread_mutex_lock(nodesToPropagateMutex);
      size = nodesToPropagate->size();
      cerr << "Thread " << pthread_self() << " unlocking nodesToPropagate (line 146)" << endl;
      pthread_mutex_unlock(nodesToPropagateMutex);
    
      continue; // see if there are more in the queue
    }
    
    cerr << "Thread " << pthread_self() << " unlocking move " << current << " (line 152)" << endl;
    pthread_mutex_unlock(&current->mutex);
    
    // compute the score of the current node
    cerr << "Thread " << pthread_self() << " locking move " << current->parent << " (line 156)" << endl;
    pthread_mutex_lock(&current->parent->mutex);
    Color madeMove = current->parent->toMove;
    cerr << "Thread " << pthread_self() << " unlocking move " << current->parent << " (line 159)" << endl;
    pthread_mutex_unlock(&current->parent->mutex);
    
    cerr << "Thread " << pthread_self() << " locking move " << current << " (line 162)" << endl;
    pthread_mutex_lock(&current->mutex);
    //double score = current->bitboard->evaluate();
    
    // generate bitboard from move tree
    Bitboard * copy = Bitboard::makeCopy(originalBitboard);
    GameTreeNode * loopPointer = current;
    while (loopPointer->parent != NULL) {
      if (loopPointer != current) {
        cerr << "Thread " << pthread_self() << " locking move " << loopPointer << " (line 171)" << endl;
        pthread_mutex_lock(&loopPointer->mutex);
      }
      copy->togglePiece((Bitboard::piece) loopPointer->pieceMoved, loopPointer->from, loopPointer->to);
      copy->previousMove = new Move(loopPointer->pieceMoved, loopPointer->from, loopPointer->to);
      copy->myColor = !copy->myColor;
      if (loopPointer != current) {
        cerr << "Thread " << pthread_self() << " unlocking move " << loopPointer << " (line 179)" << endl;
        pthread_mutex_unlock(&loopPointer->mutex);
      }
      loopPointer = loopPointer->parent;
    }
    // evaluate it
    double score = copy->evaluate();
    
    delete copy;
    
    current->hasMoveScore = true;
    current->moveScore = score;
    
    cerr << "Thread " << pthread_self() << " unlocking move " << current << " (line 189)" << endl;
    pthread_mutex_unlock(&current->mutex);
    
    cerr << "Thread " << pthread_self() << " locking move " << current->parent << " (line 192)" << endl;
    pthread_mutex_lock(&current->parent->mutex);
    
    if (current->parent->hasMoveScore == false) {
      // set the parent's score to this score
      current->parent->hasMoveScore = true;
      current->parent->moveScore = score;
    }
    else {
      if (madeMove == WHITE) {
        // higher scores are better
        if (score > current->parent->moveScore) {
          current->parent->moveScore = score;
        }
      }
      else {
        // lower scores are better
        if (score < current->parent->moveScore) {
          current->parent->moveScore = score;
        }
      }
    }
    
    // remove this child from the parent's children
    current->parent->numChildren--;
    
    // if the parent has no children left, put it on the queue to propagate up
    if (current->parent->numChildren == 0) {
      cerr << "Thread " << pthread_self() << " locking nodesToPropagate (line 220)" << endl;
      pthread_mutex_lock(nodesToPropagateMutex);
      nodesToPropagate->push_back(current->parent);
      cerr << "Thread " << pthread_self() << " unlocking nodesToPropagate (line 223)" << endl;
      pthread_mutex_unlock(nodesToPropagateMutex);
    }
    
    cerr << "Thread " << pthread_self() << " unlocking move " << current->parent << " (line 227)" << endl;
    pthread_mutex_unlock(&current->parent->mutex);
    
    // update size
    cerr << "Thread " << pthread_self() << " locking nodesToPropagate (line 231)" << endl;
    pthread_mutex_lock(nodesToPropagateMutex);
    size = nodesToPropagate->size();
    cerr << "Thread " << pthread_self() << " unlocking nodesToPropagate (line 234)" << endl;
    pthread_mutex_unlock(nodesToPropagateMutex);
  }
  
  // to avoid the compiler warning
  // "control reaches end of non void function"
  cerr << "Thread " << pthread_self() << " leaving propagateScores() (line 240)" << endl;
  return NULL;
}

void * GameTreeNode::expandGameTree(void * args) {
  struct expandGameTreeArgs * egta = (struct expandGameTreeArgs *) args;
  
  assert(egta != NULL);
  
  vector<GameTreeNode *> *nodesToExpand = egta->nodesToExpand;
  pthread_mutex_t *nodesToExpandMutex = egta->nodesToExpandMutex;
  vector<GameTreeNode *> *nodesToPropagate = egta->nodesToPropagate;
  pthread_mutex_t *nodesToPropagateMutex = egta->nodesToPropagateMutex;
  Bitboard *originalBitboard = egta->originalBitboard;
  
  assert(nodesToExpand != NULL);
  assert(nodesToExpandMutex != NULL);
  assert(nodesToPropagate != NULL);
  assert(nodesToPropagateMutex != NULL);
  assert(originalBitboard != NULL);
  
  pthread_mutex_lock(nodesToExpandMutex);
  unsigned int size = nodesToExpand->size();
  pthread_mutex_unlock(nodesToExpandMutex);
  
  while (size > 0) {
    // pop a node off the stack
    GameTreeNode * current = NULL;
    pthread_mutex_lock(nodesToExpandMutex);
    if (nodesToExpand->size() > 0) {
      //current = nodesToExpand->back();
      //nodesToExpand->pop_back();
      
      // pop from the front
      current = nodesToExpand->at(0);
      nodesToExpand->erase(nodesToExpand->begin());
      
      pthread_mutex_unlock(nodesToExpandMutex);
    }
    else {
      pthread_mutex_unlock(nodesToExpandMutex);
      break;
    }
    
    assert(current != NULL);
    
    // generate all possible moves, attach them as children
    unsigned char i;
  
    unsigned char iStart = 0;
    unsigned char iRange = 6;
    
    pthread_mutex_lock(&current->mutex);
    
    if (current->toMove == BLACK)
      iStart += 6;
      
    // generate bitboard from move tree
    Bitboard * copy = Bitboard::makeCopy(originalBitboard);
    GameTreeNode * loopPointer = current;
   	
    while (loopPointer->parent != NULL) {
      if (loopPointer != current)
        pthread_mutex_lock(&loopPointer->mutex);
      copy->togglePiece((Bitboard::piece) loopPointer->pieceMoved, loopPointer->from, loopPointer->to);
      copy->previousMove = new Move(loopPointer->pieceMoved, loopPointer->from, loopPointer->to);
      copy->myColor = !copy->myColor;
      if (loopPointer != current)
        pthread_mutex_unlock(&loopPointer->mutex);
      loopPointer = loopPointer->parent;
    }
  
    for (i=iStart; i<(iStart + iRange); i++) {
      unsigned long long * from = Bitboard::separatePieces(copy->board[i]);
      unsigned char j;
      for (j=0; j<64; j++) {
        // skip over empty
        if (from[j] == 0LLU)
          continue;
      
        // from[j] has a piece in it
        unsigned long long moves = copy->getSinglePieceMoves(current->toMove, i, from[j]);
      
        if (moves == 0LLU) {
          //cout << "DEBUG: piece " << pieceNames[i] << " has no moves." << endl;
          continue;
        }
      
        unsigned long long * to = Bitboard::separatePieces(moves);
        
        // make each move, test how good it is
        unsigned char k;
        for (k=0; k<64; k++) {
          if (to[k] == 0LLU)
            continue;
            
          //Move move(i, from[j], to[k]);
          //Bitboard * nextMove = new Bitboard(*currentGame, move);
          //Bitboard * nextMove = new Bitboard(copy, i, from[j], to[k]);
          
          GameTreeNode * child = new GameTreeNode(current->depth - 1, !current->toMove, i, from[j], to[k]);
          child->parent = current;
          current->children.push_back(child);
          current->numChildren++;
          
          // if depth > 0, put the child on the stack
          if (child->depth > 0) {
            pthread_mutex_lock(nodesToExpandMutex);
            nodesToExpand->push_back(child);
            pthread_mutex_unlock(nodesToExpandMutex);
          }
          else {
            assert(child->depth == 0);
            
            // if depth == 0, put the child in the propagate list
            // because it's at the bottom of the tree (leaf)
            pthread_mutex_lock(nodesToPropagateMutex);
            nodesToPropagate->push_back(child);
            pthread_mutex_unlock(nodesToPropagateMutex);
          }
        }
      }
      
    }
  
    pthread_mutex_unlock(&current->mutex);
    
    delete copy;
    
    // update the loop variable
    pthread_mutex_lock(nodesToExpandMutex);
    size = nodesToExpand->size();
    pthread_mutex_unlock(nodesToExpandMutex);
    
  }
  
  // to avoid the compiler warning
  // "control reaches end of non void function"
  return NULL;
}

std::string GameTreeNode::toString() {
  string fromString = Bitboard::squareToString(from);
  string toString = Bitboard::squareToString(to);
  stringstream ss;
  ss << fromString << toString;
  string output;
  ss >> output;
  return output;
}

void GameTreeNode::debugGameTree(GameTreeNode * root) {
  assert(root != NULL);
  
  ofstream outFile;
  
  // recursively print the whole game tree in graphviz format
  outFile.open("debug_game_tree.dot");
  outFile << "digraph unix {" << endl;
  outFile << "\tsize=\"6,6\";" << endl;
  outFile << "\tnode [color=lightblue2, style=filled];" << endl;
  outFile.close();
  
  debugGameTreeRecursive(root);
  
  outFile.open("debug_game_tree.dot", ios::app);
  outFile << "}" << endl;
  outFile.close();
}

void GameTreeNode::debugGameTreeRecursive(GameTreeNode * root) {
  assert(root != NULL);
  
  ofstream outFile;
  
  // print self with color
  outFile.open("debug_game_tree.dot", ios::app);
  outFile << "\t" << root->toString() << " [color=\"";
  if (root->hasMoveScore)
    outFile << "skyblue";
  else
    outFile << "red";
  outFile << "\"];" << endl;
  outFile.close();
  
  // print links between the root of the (sub)-tree and its children
  unsigned int i;
  for (i=0; i<root->children.size(); i++) {
    GameTreeNode *child = root->children.at(i);
    assert(child != NULL);
    
    outFile.open("debug_game_tree.dot", ios::app);
    outFile << "\t" << root->toString() << " -> ";
    outFile << child->toString() << ";" << endl;
    outFile.close();
    
    debugGameTreeRecursive(child);
  }
}
