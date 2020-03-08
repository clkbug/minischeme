#!/bin/bash
set -eu

tests=(atoms.scm write-simple.scm write-quote.scm)

for t in ${tests[@]}; do
    echo $t
    diff <(gosh test/$t) <(gosh main.scm test/$t)
    echo $t ... OK
done
