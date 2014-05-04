#! /bin/bash
cp cibic testbed/
cp lib.s testbed/
cd testbed/
for f in *.c
do
    echo $f
    ./cibic $f > mips.s 2> /dev/null
    gcc $f -m32 -std=c99 2> /dev/null
    ./spim -stat -file mips.s > out
    ./a.out > std
    diff std out
    if [[ "$?" != 0 ]]; then
        echo "Wrong Answer!"
        break;
    fi
done
echo "OK."
