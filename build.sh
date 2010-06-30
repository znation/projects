#!/bin/sh
gcc \
-W \
-Wall \
-Wundef \
-Wstrict-prototypes \
-Wmissing-prototypes \
-Wmissing-declarations \
-pg \
-o analyzequotes.exe \
analyzequotes.c
