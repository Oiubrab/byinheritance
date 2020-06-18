#!bin/bash

if [ -f 'heartwork/neurotic.txt' ]; then rm heartwork/neurotic.txt; fi
if [ -f 'neurotic/heartwork.txt' ]; then rm neurotic/heartwork.txt; fi

reset

#network scaling is the fractional effect that the brain has in maximising the blood transitions
#blood scaling is the fractional effect that the blood has in maximising the brain transitions

if [ "$#" -ne 8 ]
then
    echo "Execute program by format:"
	echo ". questions.sh valves cycles maximum_columns maximum_rows lag blood_scaling network_scaling printed"
	echo "valves: closed open bottom_open"
	echo "printed: yes no debug"

else

	cd heartwork
	pgfortran -traceback -Mcuda -Minfo=all flesh.f95 blood.f95 -o megalomaniac_blood
	cd ../neurotic
	pgfortran -traceback -Mcuda discrete_flesh.f95 network.f95 -o megalomaniac_network
	cd ..

	for i in $(seq 1 $2)
	do

		cd heartwork
		./megalomaniac_blood $3 $4 $6 $8
		if [ -f 'network.txt' ]; then rm network.txt; fi
		mv heartwork.txt ../neurotic
		cd ..

		cd neurotic
		./megalomaniac_network $1 $3 $4 9 9 $5 $7 $8
		if [ -f 'heartwork.txt' ]; then rm heartwork.txt; fi
		mv neurotic.txt ../heartwork
		cd ..

	done
	
fi
