#!/bin/zsh

#this runner runs the finance version of the brain
# three arguments: 
#first argument: cleans prior runs (clean/noclean)
#second argument: runs the system this many times
#third argument: includes the test output (test/notest)

reset

#first, remove the old binary and module files
if [[ $2 = "test" ]]
then
	rm lack_of_comprehension in_search_of_sanity.mod spiritechnology.mod welcome_to_dying.mod the_sound_of_perserverance.mod darkness.mod
elif [[ $2 = "notest" ]]
then
	rm lack_of_comprehension in_search_of_sanity.mod spiritechnology.mod welcome_to_dying.mod the_sound_of_perserverance.mod
else
	exit
fi

# this option removes all prior brain and market data, allowing a brand new run to take place
if [[ $1 = "clean" ]]
then

	#remove the previous network and logs
	rm *.txt
elif [[ $1 != "noclean" ]]
then
	exit
fi

#compile
caf -fbounds-check welcome_to_dying.f95 the_sound_of_perserverance.f95 darkness.f95 spiritechnology.f95 in_search_of_sanity.f95 at_the_heart_of_winter.f95 -o lack_of_comprehension
#run the game
cd exponential_uncertainty_death_machine
./exponential_uncertainty_death_machine.x86_64 &
cd ..

#the last trapped fury
cafrun -n 2 --use-hwthread-cpus ./lack_of_comprehension $2
