#!/bin/bash

# FIXME: you kinda need to process the test files into real sms that stepmania
# understands, so the SL-ChartParser parser will also understand them. sry ><
TESTS1="$HOME/stepmania/Songs/zzz-xover-test/test-*/*.sm"
TESTS2="$HOME/stepmania/Songs/yyy-bracker-test/test-*/*.sm"
# SIGBOVIKDIR="$HOME/sigbovik/itg/code"

function success() {
	echo -e "\033[01;32m$1\033[00m"
}
function warn() {
	echo -e "\033[01;33m$1\033[00m"
}
function err() {
	echo -e "\033[01;31m$1\033[00m"
}

total=0
passed=0
passed_tests=""
for sm in `ls $TESTS1 $TESTS2`; do
	filename=`basename "$sm"`
	# testname="$SIGBOVIKDIR/$filename"
	testname=$filename
	if [ ! -f "$testname" ]; then
		warn "$testname missing in sigbovik dir, skipping"
		continue
	fi
	# echo -ne "testing $filename... "

	expected=`head -n 1 "$testname"`
	result=`lua test-harness.lua "$sm" | tail -n 1`
	if echo "$result" | grep "$expected" >/dev/null; then
		# pass
		success "[ ok ] $filename"
		pass=$(($pass+1))
		passed_tests="$passed_tests $filename"
	else
		err "[fail] $filename - expected $expected got $result"
	fi
	total=$(($total+1))
done

if [ "$passed" = "$total" ]; then
	success "\u2728\u2b50\u2728 all $total tests passed \u2728\u2b50\u2728"
else
	warn "$pass/$total passed; subset checksum `echo "$passed_tests" | md5sum`"
fi
