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
-o analyzequotes.exe \
analyzequotes.c \
-lncurses
