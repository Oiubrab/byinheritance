#!/bin/zsh

#this runner runs the finance version of the brain
#two arguments:
 

reset

broke=0

#first, remove the old binary and module files
if [[ $2 = "test" ]] || [[ $2 = "notest" ]]
then
	rm lack_of_comprehension in_search_of_sanity.mod spiritechnology.mod welcome_to_dying.mod the_sound_of_perserverance.mod
else
	broke=1	
fi

# this option removes all prior brain and market data, allowing a brand new run to take place
if [[ $1 = "clean" ]]
then

	#remove the previous network and logs
	rm *.txt
elif [[ $1 != "noclean" ]]
then
	broke=1
fi

#tell the user what to do if they fuck up
if [[ $broke = 1 ]]
then
	echo "run this with two arguments like:"
	echo "./i_am_in_command arg1 arg2"
	echo "where:"
	echo "arg1=clean/noclean (removes all txt files in the rooting directory"
	echo "arg2=test/notest (causes the AI to generated testing txt files)"
	unset $broke
	exit
else
	unset $broke
fi
	

#compile
caf -fbounds-check welcome_to_dying.f95 the_sound_of_perserverance.f95 spiritechnology.f95 in_search_of_sanity.f95 at_the_heart_of_winter.f95 -o lack_of_comprehension
#run the game
cd exponential_uncertainty_death_machine
./exponential_uncertainty_death_machine.x86_64 &
cd ..

#the last trapped fury
cafrun -n 2 --use-hwthread-cpus ./lack_of_comprehension $2
