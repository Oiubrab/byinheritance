#!/bin/zsh

#this runner runs the finance version of the brain
# three arguments: 
#first argument: turns the testing output on (true) or off (false)
#second argument: turns blood printing in test output on (true) or off (false)
#optional third argument: cleans prior runs

#first, setup the binary
reset
rm in_search_of_sanity spiritechnology.mod welcome_to_dying.mod
# this option cleans out the brain, ready for a new run
if [[ $3 = "clean" ]]
then
	rm *.txt
fi
caf -fbounds-check libcsv.a welcome_to_dying.f95 spiritechnology.f95 in_search_of_sanity.f95 -o in_search_of_sanity
cafrun -n 2 --use-hwthread-cpus ./in_search_of_sanity $1 $2

