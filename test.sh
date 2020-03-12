#!/bin/bash
set -eu

tests=( \
	atoms.scm \
	    write-simple.scm \
	    write-quote.scm \
	    cons.scm \
	    arith-simple.scm \
	    begin.scm \
	    lambda-simple.scm \
	    if.scm \
	    lambda-closure.scm \
	    define-toplevel.scm \
    )

for t in ${tests[@]}; do
    echo $t
    diff <(gosh test/$t) <(gosh main.scm test/$t)
    echo $t ... OK
done
