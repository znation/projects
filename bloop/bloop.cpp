#include <stdio.h>
#include <iostream>
#include <istream>
#include <sys/time.h>
#include <time.h>    // for time()
#include "BloopEngine.h"
#include "Bitboard.h"
#include "GameTreeNode.h"

using namespace std;

int main(int argc, char ** argv) {

  srand(time(NULL));  // Initialize random number generator.
  
  // initialize attack arrays
  Bitboard::preComputeAttacks();
  
  // look at sizes of various structures
  /*
  cout << "---<DEBUG>---" << endl;
  
  cout << "int is " << sizeof(int) << endl;
  cout << "Bitboard is " << sizeof(Bitboard) << endl;
  cout << "GameTreeNode is " << sizeof(GameTreeNode) << endl;
  
  cout << "---</DEBUG>--" << endl;
  */

  Color engineColor = BLACK;

  bool noxboard = false;
  int i;
  for (i=0; i<argc; i++) {
    if (strcmp(argv[i], "-noxboard") == 0)
      noxboard = true;
    else if (strcmp(argv[i], "-threadtest") == 0) {
      // run a multithreading test
      
      cout << "Depth\t\tThreads\t\tCPU time\tReal time" << endl;
      
      int j;
      for (j=1; j<5; j++) {
        // search depth is j
        int k;
        for (k=1; k<8; k++) {
          // numThreads is k
          
          Bitboard::depth = j;
          Bitboard::numThreads = k;
          
          Bitboard * bb = new Bitboard(BLACK);
          bb->processUserMove("e2e4");
      
          struct timeval tvStart;
          struct timeval tvEnd;
          
          gettimeofday(&tvStart, NULL);
          clock_t start = clock();
      
          bb->makeBestMove(BLACK);
      
          gettimeofday(&tvEnd, NULL);
          clock_t stop = clock();
      
          double cpuTimeUsed = ((double)stop-(double)start)/(double)CLOCKS_PER_SEC;
          
          double realTimeUsed = ((double)tvEnd.tv_sec - (double)tvStart.tv_sec)
            + (((double)tvEnd.tv_usec - (double)tvStart.tv_usec)/1000000.0);
      
          cout << j << "\t\t";
          cout << k << "\t\t";
          cout << cpuTimeUsed << "\t\t";
          cout << realTimeUsed << endl;
          
          delete bb;
          
        }
      }
      
      
      
      return EXIT_SUCCESS;
    }
  }
  
  // initialize engine
  BloopEngine engine;
  
  if (noxboard) {
    engine = BloopEngine(false);
    
    // play command line
    engine.newGame(engineColor);
    
    string move;
    while (move != "quit") {
      // prompt for move
      cout << "Your move?" << endl;
      cin >> move;
      if (move != "quit")
        engine.makeUserMove(move);
    }
    
    return 0;
  }
  
  // play xboard
  engine = BloopEngine();

  // Make IO unbuffered for use with XBoard  
  cout.setf(ios::unitbuf);
  //cout.rdbuf()->setbuf(NULL, 0);
  //cin.rdbuf()->setbuf(NULL, 0);
  
  while (cin.rdbuf()->in_avail() != -1)
  {
    // -1 means no more input?
    string command;
    getline(cin, command);
    
    //cout << "Error (debug): command was \"" << command << "\"" << endl;
    
    if (command == "quit") {
    	return 0;
    }
    
    else if (command == "xboard" || command.substr(0, 8) == "protover") {
      cout << "feature done=0" << endl;
      cout << "feature ping=0" << endl;
      cout << "feature myname=\"Bloop\"" << endl;
      cout << "feature variants=\"normal\"" << endl;
      cout << "feature pause=0" << endl;
      cout << "feature sigint=0" << endl;
      cout << "feature sigterm=0" << endl;
      cout << "feature setboard=1" << endl;
      cout << "feature playother=0" << endl;
      cout << "feature analyze=0" << endl;    // TODO: analyze mode
      cout << "feature name=0" << endl;
      cout << "feature san=0" << endl;
      cout << "feature usermove=1" << endl;
      cout << "feature done=1" << endl;
    }
    
    else if (command.substr(0, 8) == "accepted") {
      // it accepted some feature from the list, ignore
    }
    
    else if (command == "new") {
      engine.newGame(engineColor);
    }
    
    // TODO: implement time controls ("level", "st") ("time", "otim")
    
    // TODO: implement undo, remove
    
    else if (command.substr(0, 8) == "setboard") {
      engine.setPosition(command.substr(9));
    }
    
    else if (command == "post") {
      engine.setPost(true);
      engine.setPostWhilePondering(true);
    }
    
    else if (command == "nopost") {
      engine.setPost(false);
      engine.setPostWhilePondering(false);
    }
    
    else if (command == "white") {
      //engineColor = WHITE;
      //engine.play(WHITE);
      
      // TODO: new game on color change breaks xboard
      // that's ok for now... don't change colors mid-game
      engine.newGame(WHITE);
    }
    
    else if (command == "black") {
      //engineColor = BLACK;
      //engine.play(BLACK);
      
      engine.newGame(BLACK);
    }
    
    else if (command == "edit") {
      cout << "Error (not implemented): edit" << endl;
    }
    
    else if (command == "bk") {
      cout << "Error (not implemented): does not have opening book yet" << endl;
    }
    
    else if (command == "analyze") { 
      cout << "Error (not implemented): analyze" << endl;
    }
    
    else if (command == "sd") {
      const char * depthStr = command.substr(3).c_str();
      engine.setMaxDepth(atoi(depthStr));
    }
    
    // TODO: implement "go"
    
    else if (command == "go") {
      engine.play();
    }
    
    else if (command.substr(0, 8) == "usermove" && command.length() > 9) {
      command = command.substr(9);
      engine.makeUserMove(command);
    }
    
    else {
      // ignore commands we don't recognize
      //cout << "Error (parsing): command \"" << command << "\" not recognized" << endl;
    }
    
  }
  
  return EXIT_SUCCESS;
}
