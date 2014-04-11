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
    if [ $? -ne $gcc_ret ]; then
        echo "Failed on $file"
    else
        echo "ok $file"
    fi
done
