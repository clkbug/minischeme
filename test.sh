#!/bin/bash
set -u

tests=(
    atoms.scm
    write-simple.scm
    write-quote.scm
    cons.scm
    arith-simple.scm
    begin.scm
    lambda-simple.scm
    if.scm
    lambda-closure.scm
    lambda-dotted.scm
    define-toplevel.scm
    quasiquote.scm
    lets.scm
    cond.scm
    main-procedure.scm
    message-passing.scm
    miniminischeme-0.scm
#	    read-and-write.scm \
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

read_and_write_values=(
    '1'
    '3'
    '#t'
    '#f'
    'aiueo'
    '(1 2 3)'
    '(1 (2 3))'
    '(a (b c (d e (f g) (h (i j (k l)) m ) n) o p) q r s)'
    '(1 . 2)'
    '(1 . (2 3 . 4))'
    '(quote 1)'
    '`(1 2 ,a ,@(hoge fuga piyo) 3)'
)
rw=test/read-and-write.scm
for val in "${read_and_write_values[@]}"; do
    echo -n "${rw} <- ${val} ... "
    result=$(diff <(echo \'${val}\' | gosh ${rw}) <(echo \'${val}\' | gosh main.scm ${rw}))
    if [ $? -ne 0 ]; then
	echo -e $result
	echo read-and-write \($val\) ... Failed
	exit 1
    else
	echo "OK"
    fi
done

# self-host test
echo
echo ==
echo start self-host test
echo --
for t in ${tests[@]}; do
    echo -n self-host $t ...
    result=$(diff <(gosh test/$t) <(gosh main.scm main.scm test/$t))

    if [ $? -ne 0 ]; then
	echo -e $result
	echo self-host $t ... Failed
	exit 1
    fi
    echo ' OK'
done
