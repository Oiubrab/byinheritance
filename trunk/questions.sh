#!bin/bash

if [ -f 'heartwork/neurotic.txt' ]; then rm heartwork/neurotic.txt; fi
if [ -f 'neurotic/heartwork.txt' ]; then rm neurotic/heartwork.txt; fi
rm -r neurotic/error_folder/neurotic_error*

reset

if [ "$#" -ne 8 ]
then
    echo "Execute program by format:"
	echo ". questions.sh valves cycles maximum_columns maximum_rows lag blood_scaling network_scaling printed"
	echo "valves: closed open bottom_open"
	echo "printed: yes no debug"

else

	cd heartwork
	pgfortran -traceback -Mcuda flesh.f95 blood.f95 -o megalomaniac_blood
	cd ../neurotic
	pgfortran -traceback -Mcuda discrete_flesh.f95 network.f95 -o megalomaniac_network
	cd ..

	for i in $(seq 1 $2)
	do
		
		
		#operate the heartwork step
		cd heartwork
		./megalomaniac_blood $3 $4 $6 $8
		#if something goes wrong, stop the process
		if [ -f 'heartwork.txt' ] 
		then
			mv heartwork.txt ../neurotic
		else
			cd ..
			echo heartwork copy error
			break
		fi
		#delete recieved networks once finished with
		if [ -f 'neurotic.txt' ]; then rm neurotic.txt; fi
		cd ..


		#operate the neurotic step
		cd neurotic
		./megalomaniac_network $1 $3 $4 9 9 $5 $7 $8
		#if something goes wrong, stop the process
		if [ -f 'neurotic.txt' ] 
		then
			mv neurotic.txt ../heartwork
		else
			cd ..
			echo neurotic copy error
			break
		fi
		#delete recieved networks once finished with
		if [ -f 'heartwork.txt' ]; then rm heartwork.txt; fi
		cd ..

	done

	#if [ -f 'heartwork/neurotic.txt' ]; then rm heartwork/neurotic.txt; fi
	#if [ -f 'neurotic/heartwork.txt' ]; then rm neurotic/heartwork.txt; fi
	
fi
