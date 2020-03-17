#!/bin/bash
set -u

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
	    lambda-dotted.scm \
	    define-toplevel.scm \
	    quasiquote.scm \
	    define-macro.scm \
	    sytax-rules-simple.scm \
	    lets.scm \
    )

for t in ${tests[@]}; do
    # echo $t
    result=$(diff <(gosh test/$t) <(gosh main.scm test/$t))
    if [ $? -ne 0 ]; then
	echo $result
	echo $t ... Failed
	exit 1
    fi
    echo $t ... OK
done
