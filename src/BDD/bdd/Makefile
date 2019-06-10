# Makefile for Reduced Ordered Boolean Decision Diagram Manipulation
# Copyright (c) 1990, 1991, 1992, 1993, 1994-1997 G. Janssen

#CC=xlc
all:
	cd utils; make CC="$(CC)"
	cd src; make CC="$(CC)"; make CC="$(CC)" lib; make CC="$(CC)" lib2
	cd draw; make CC="$(CC)"

clean:
	cd utils; make clean
	cd src; make clean
	cd draw; make clean
