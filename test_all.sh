#! /bin/bash
dir=testcases/*.c
if [ "$#" != 0 ]; then
    dir=$1
fi
for file in $dir
do
    gcc $file -o /dev/null &> /dev/null
    gcc_ret="$?"
    ./cibic $file &> /dev/null
    ret=$?
    if [ $ret -ne $gcc_ret ]; then
        echo "Failed on $file"
    else
        echo "ok $file: $ret"
    fi
done
