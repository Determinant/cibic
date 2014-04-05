#! /bin/bash
for file in test/*
do
    gcc $file -o /dev/null &> /dev/null
    gcc_ret="$?"
    ./cibic $file &> /dev/null
    if [ $? -ne $gcc_ret ]; then
        echo "Failed on $file"
        break
    fi
done
