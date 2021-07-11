#!/bin/zsh

#this runner runs the finance version of the brain
# three arguments: 
#first argument: cleans prior runs (clean/noclean)
#second argument: runs the system this many times
#third argument: includes the test output (test/notest)

#first, remove the old binary and module files

#test for printing
if [[ $4 != "print" && $4 != "noprint" ]]
then
	exit
elif [[ $4 = "print" ]]
then
	reset
fi

if [[ $3 = "test" ]]
then
	rm lack_of_comprehension in_search_of_sanity.mod spiritechnology.mod welcome_to_dying.mod the_sound_of_perserverance.mod darkness.mod
elif [[ $3 = "notest" ]]
then
	rm lack_of_comprehension in_search_of_sanity.mod spiritechnology.mod welcome_to_dying.mod the_sound_of_perserverance.mod
else
	exit
fi

# this option removes all prior brain and market data, allowing a brand new run to take place
if [[ $1 = "clean" ]]
then

	#reset the market
	cd world_in_a_world
	python3 account_clean.py3
	setopt extended_glob && rm -- ^*(.py3|.py|reset.csv|stocks|__pycache__)
	echo 0.0 > holding_sum.txt
	python3 real_price_generator.py3 1 $2
	python3 test_market.py3 reset $4
	cd ..
	#remove the previous network and logs
	rm *.txt
elif [[ $1 != "noclean" ]]
then
	exit
fi

#compile
caf -fbounds-check welcome_to_dying.f95 the_sound_of_perserverance.f95 darkness.f95 spiritechnology.f95 in_search_of_sanity.f95 at_the_heart_of_winter.f95 -o lack_of_comprehension


#run the network $4 times
for i in $(seq 1 $2)
do

	if [[ $4 = "print" ]]
	then 
		echo "run: " $i | tee -a "test_log.txt"
	fi

	cafrun -n 4 --use-hwthread-cpus ./lack_of_comprehension $3

	cd world_in_a_world
	python3 real_price_generator.py3 $i $2 carryon
	python3 test_market.py3 carryon $4 | tee -a "../test_log.txt"
	cd ..
	
done

