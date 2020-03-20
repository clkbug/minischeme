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
	    lets.scm \
	    cond.scm \
#	    define-macro.scm \
#	    sytax-rules-simple.scm \

    )

for t in ${tests[@]}; do
    echo -n $t ...
    result=$(diff <(gosh test/$t) <(gosh main.scm test/$t))
    if [ $? -ne 0 ]; then
	echo -e $result
	echo $t ... Failed
	exit 1
    fi
    echo ' OK'
done
