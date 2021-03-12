#!/bin/bash

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

	#reset the market
	cd world_in_a_world
	python3 test_market.py3 reset
	cd ..
	#remove the previous network and logs
	rm *.txt
fi

#compile
caf -fbounds-check welcome_to_dying.f95 spiritechnology.f95 in_search_of_sanity.f95 -o in_search_of_sanity

#run the network $4 times
for i in $(seq 1 $4)
do
	
	cafrun -n 2 --use-hwthread-cpus ./in_search_of_sanity $1 $2
	cd world_in_a_world
	python3 test_market.py3 carryon
	cd ..
	echo "run: " $i
	
done

#clean the account and archive
cd world_in_a_world
python3 account_clean.py3
cd ..
