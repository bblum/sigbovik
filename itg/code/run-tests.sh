#!/bin/bash

rv=0

if [ -z "$1" ]; then
	TESTS=`ls test*.sm`
else
	TESTS=`ls *$1* 2>/dev/null | grep '\.sm$'`
	if [ -z "$TESTS" ]; then
		echo -e "\033[01;33mno such test matching *$1*(.sm)\033[00m"
		exit 1
	fi
fi

for t in $TESTS; do
	ans=`head -n 1 "$t"`
	if ! echo "$ans" | grep ",.*,.*," >/dev/null; then
		echo -e "\033[01;33m$t not well formed -- $ans not an answer.\033[00m"
		continue
	fi
	out=`cat "$t" | ./ITG | sed 's/\t/,/g'`
	if echo "$out" | grep "$ans" >/dev/null; then
		echo -e "\033[01;32m[ OK ]\033[00m $t"
	else
		echo -e "\033[01;31m[FAIL] $t -- expected: $ans; got: $out\033[00m"
		rv=1
	fi
done
exit $rv
