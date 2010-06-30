#!/bin/sh

#ghc --make analyzequotes.hs -prof -auto-all -Wall -fforce-recomp -fglasgow-exts -O3

gcc -Wall -o analyzequotes.exe analyzequotes.c
