#!/bin/sh

# Provides a convenient way to run befunge programs
# Requires sbcl

cd `dirname $0`

if [ $# -eq 1 ];
then
	if [ -e $1 ]
	then
		sbcl --script puffball.lisp $1
	else
		echo Error: $1 is not a file
	fi
else
	echo "Usage: puffball.sh [filename]"
fi
