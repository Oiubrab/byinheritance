#!/bin/zsh

#this runner runs the finance version of the brain
# four arguments: 
#first argument: turns the testing output on (true) or off (false)
#second argument: turns blood printing in test output on (true) or off (false)
#third argument: cleans prior runs
#fourth argument: runs the system this many times

#first, remove the old binary and module files
reset
rm lack_of_comprehension in_search_of_sanity.mod spiritechnology.mod welcome_to_dying.mod reign_in_blood.mod
# this option removes all prior brain and market data, allowing a brand new run to take place
if [[ $3 = "clean" ]]
then

	#reset the market
	cd world_in_a_world
	echo 0.0 > holding_sum.txt
	python3 test_market.py3 reset
	cd ..
	#remove the previous network and logs
	rm *.txt
fi

#compile
caf -fbounds-check welcome_to_dying.f95 reign_in_blood.f95 spiritechnology.f95 in_search_of_sanity.f95 at_the_heart_of_winter.f95 -o lack_of_comprehension

#run the network $4 times
for i in $(seq 1 $4)
do
	
	cafrun -n 3 --use-hwthread-cpus ./lack_of_comprehension $1 $2
	cd world_in_a_world
	python3 test_market.py3 carryon | tee -a "../test_log.txt"
	cd ..
	echo "run: " $i | tee -a "test_log.txt"
	
done

#clean the account and archive
cd world_in_a_world
python3 account_clean.py3
cd ..
