#!/bin/zsh

#this runner runs the finance version of the brain
# two arguments: 
#first argument: cleans prior runs
#second argument: runs the system this many times

#first, remove the old binary and module files
reset
rm lack_of_comprehension in_search_of_sanity.mod spiritechnology.mod welcome_to_dying.mod reign_in_blood.mod
# this option removes all prior brain and market data, allowing a brand new run to take place
if [[ $1 = "clean" ]]
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
for i in $(seq 1 $2)
do
	
	echo "run: " $i | tee -a "test_log.txt"
	cafrun -n 4 --use-hwthread-cpus ./lack_of_comprehension
	cd world_in_a_world
	python3 real_price_generator.py3 $i $2
	python3 test_market.py3 carryon | tee -a "../test_log.txt"
	cd ..
	
done

#clean the account and archive
cd world_in_a_world
python3 account_clean.py3
cd ..
