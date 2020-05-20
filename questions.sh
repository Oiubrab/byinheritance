#!bin/bash

if [ -f 'heartwork/neurotic.txt' ]; then rm heartwork/neurotic.txt; fi
if [ -f 'neurotic/heartwork.txt' ]; then rm neurotic/heartwork.txt; fi

reset

if [ "$#" -ne 6 ]
then
    echo "Execute program by format:"
	echo ". questions.sh valves cycles maximum_columns maximum_rows lag printed"
	echo "valves: closed open bottom_open"
	echo "printed: yes no debug"

else

	cd heartwork
	pgfortran -traceback flesh.f95 blood.f95 -o megalomaniac_blood
	cd ../neurotic
	pgfortran -traceback discrete_flesh.f95 network.f95 -o megalomaniac_network
	cd ..

	for i in $(seq 1 $2)
	do

		cd heartwork
		./megalomaniac_blood $3 $4 $6
		if [ -f 'network.txt' ]; then rm network.txt; fi
		mv heartwork.txt ../neurotic
		cd ..

		cd neurotic
		./megalomaniac_network $1 $3 $4 1 15 $5 $6
		if [ -f 'heartwork.txt' ]; then rm heartwork.txt; fi
		mv neurotic.txt ../heartwork
		cd ..

	done

	if [ -f 'heartwork/neurotic.txt' ]; then rm heartwork/neurotic.txt; fi
	if [ -f 'neurotic/heartwork.txt' ]; then rm neurotic/heartwork.txt; fi
	
fi
