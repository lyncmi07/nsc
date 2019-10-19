#!/bin/sh

stack install

for i in $(find ./test/*.ns)
do
    if cat $i | no-syn-exe ; then
        echo "$?"
    else
        echo "The test $i failed"
        exit 1
    fi
done

for i in $(find ./test/*.ns.fail)
do
    echo "Running test $i"
    if cat $i | no-syn-exe ; then
        echo "The test $i should have failed but did not"
        exit 1
    else
        echo $?
    fi
done
