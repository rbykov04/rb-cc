#!/bin/bash
tmp=`mktemp -d /tmp/rb-cc-test-XXXXXX`
trap 'rm -rf $tmp' INT TERM HUP EXIT
echo > $tmp/empty.c
check() {
    if [ $? -eq 0 ]; then
        echo "testing $1 ... passed"
    else
        echo "testing $1 ... failed"
        exit 1
    fi
}
# --help
./rb-cc --help 2>&1 | grep -q rb-cc
check --help

# -o
#rm -f $tmp/out
#./rb-cc -o $tmp/out $tmp/empty.c
#[ -f $tmp/out ]
#check -o
