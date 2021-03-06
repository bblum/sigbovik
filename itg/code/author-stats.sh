#!/bin/bash
sheet=boring.tsv
author="$1"

# XXX
if [ "$author" = "(-[Jayce]-)" ]; then
	author="...Jayce..."
fi

# strip filename, song name, filter by author
content=`cat "$sheet" | sed 's/^[^\t]*\t[^\t]*\t//' | grep "^$author"`

# exclude 1s/2s/3s, strip author, chart name, and difficulty
# grepping for tabs is horrible :(
content=`echo "$content" | sed 's/^[^\t]*\t[^\t]*\t[^\t]*\t//' | grep -ve "^1$(printf '\t')\|^2$(printf '\t')\|^3$(printf '\t')"`
stats=`echo "$content" | sed 's/^[^\t]*\t//'`

# grab each column and turn into a line for bc
steps=`echo "$stats" | sed 's/\t.*//' | tr '\n' '+' | sed 's/$/0/'`
xovers=`echo "$stats" | sed 's/^[^\t]*\t//' | sed 's/\t.*//' | tr '\n' '+' | sed 's/$/0/'`
switches=`echo "$stats" | sed 's/^[^\t]*\t[^\t]*\t//' | sed 's/\t.*//' | tr '\n' '+' | sed 's/$/0/'`
jacks=`echo "$stats" | sed 's/^[^\t]*\t[^\t]*\t[^\t]*\t//' | sed 's/\t.*//' | tr '\n' '+' | sed 's/$/0/'`
doubles=`echo "$stats" | sed 's/^[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t//' | sed 's/\t.*//' | tr '\n' '+' | sed 's/$/0/'`
sidefoots=`echo "$stats" | sed 's/^[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t//' | sed 's/\t.*//' | tr '\n' '+' | sed 's/$/0/'`

charts=`echo "$stats" | wc -l`
steps=`echo "$steps" | bc`
xovers=`echo "$xovers" | bc`
switches=`echo "$switches" | bc`
jacks=`echo "$jacks" | bc`
doubles=`echo "$doubles" | bc`
sidefoots=`echo "$sidefoots" | bc`

echo -e "$author\t$charts\t$steps\t$xovers\t$switches\t$jacks\t$doubles\t$sidefoots"
