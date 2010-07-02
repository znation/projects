#!/bin/sh
gcc \
-W \
-Wall \
-Wundef \
-Wstrict-prototypes \
-Wmissing-prototypes \
-Wmissing-declarations \
-pg \
-g \
-O2 \
-o analyzequotes.exe \
analyzequotes.c \
-lncurses
