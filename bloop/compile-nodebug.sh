#!/bin/sh
g++ -Wall -O2 -DNDEBUG -lpthread *.cpp -o bloop
strip bloop
