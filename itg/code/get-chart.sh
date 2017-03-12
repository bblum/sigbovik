#!/bin/sh

FILE="$1"
if [ ! -f "$FILE" ]; then
	echo "no such file $file"
	exit 1
fi

function process() {
	FOUND_SINGLES_CHART=0
	NEXT_LINE_SINGLES_CHART=0 # used for finding step artist
	FOUND_DIFFICULTY=0
	NEXT_LINE_DIFFICULTY=0 # used for finding difficulty rating
	MAYBE_STEP_ARTIST=
	STEP_ARTIST=
	NUM_FEET=
	CHART_CONTENT=
	while read line; do
		if [ "$FOUND_DIFFICULTY" = 1 ]; then
			if [ "$NEXT_LINE_DIFFICULTY" = 1 ]; then
				# Difficulty category was found immediately previous line; number of feet will be here.
				NUM_FEET=`echo "$line" | sed 's/^ *//' | sed 's/:$//' | sed 's/:\r$//'`
				NEXT_LINE_DIFFICULTY=0
			elif echo "$line" | grep "dance-.*:" >/dev/null; then
				# End of our chart.
				break;
			else
				# Line in middle of our chart. Print it!
				CHART_CONTENT="${CHART_CONTENT}${line}\n"
			fi
		elif [ "$FOUND_SINGLES_CHART" = 1 ]; then
			if [ "$NEXT_LINE_SINGLES_CHART" = 1 ]; then
				# "dance-single:" was found immediately previous line. Artist will be here.
				MAYBE_STEP_ARTIST=`echo "$line" | sed 's/^ *//' | sed 's/:$//' | sed 's/:\r$//'`
				NEXT_LINE_SINGLES_CHART=0
			elif echo "$line" | grep "$DIFF:" >/dev/null; then
				# "dance-single:" was found a while ago. Look for difficulty category.
				FOUND_DIFFICULTY=1
				NEXT_LINE_DIFFICULTY=1
				STEP_ARTIST="$MAYBE_STEP_ARTIST"
			fi
		fi
		if echo "$line" | grep "dance-single:" >/dev/null; then
			FOUND_SINGLES_CHART=1
			NEXT_LINE_SINGLES_CHART=1
		fi

	done
	if [ "$FOUND_DIFFICULTY" = 1 ]; then
		SHORTFILE=`echo "$FILE" | sed 's/.*stepmania.Songs.//'`
		echo -ne "$SHORTFILE\t$STEP_ARTIST\t$DIFF\t$NUM_FEET\t"
		echo -e "$CHART_CONTENT" | ./itg
	fi
}

for DIFF in Beginner Easy Medium Hard Challenge Edit; do
	if grep "$DIFF:" "$FILE" >/dev/null; then
		grep -A1000000000 "$DIFF:" "$FILE" | process
	fi
done
