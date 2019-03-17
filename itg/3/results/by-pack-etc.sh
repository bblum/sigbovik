#!/bin/bash

IFS=$'\n'

input=by-pack-etc.txt

# field 2 = pack
# field 3 = year
# field 4 = author
key=4

# extract the key
keys=`cat $input | cut -d'|' -f$key | sort | uniq`

#echo "there are `echo "$keys" | wc -l` keys"
for key in $keys; do
	matches=`cat $input | grep "|$key|"`
	entries=`echo "$matches" | wc -l`
	step_bc=`echo "$matches" | cut -d'|' -f5 | tr '\n' '+' | sed 's/+$//'` # dont know why i need to separate this; IFS maybe?
	steps=`echo "$step_bc" | bc`
	jump_bc=`echo "$matches" | cut -d'|' -f6 | tr '\n' '+' | sed 's/+$//'`
	jumps=`echo "$jump_bc" | bc`
	bj_bc=`echo "$matches" | cut -d'|' -f7 | tr '\n' '+' | sed 's/+$//'`
	bjs=`echo "$bj_bc" | bc`
	# output keyname - num_entries - total_steps - total_jumps - total_bjs - bjs% - bjj%
	echo -e "$key\t$entries\t$steps\t$jumps\t$bjs"
done
