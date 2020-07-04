#!bin/bash

if [ -f 'heartwork/will.txt' ]; then rm heartwork/will.txt; fi
if [ -f 'neurotic/heartwork.txt' ]; then rm neurotic/heartwork.txt; fi
if [ -f 'will/neurotic.txt' ]; then rm will/neurotic.txt; fi
rm -r neurotic/error_folder/neurotic_error*

reset

if [ "$#" -ne 9 ]
then
    echo "Execute program by format:"
	echo ". questions.sh valves valve_value cycles maximum_columns maximum_rows lag blood_scaling network_scaling printed"
	echo "valves: left right up down custom"
	echo "printed: yes no debug network_only"
	echo "network_scaling: scales the amount blood neurons will increase the weights that lead to brain neurons"
	echo "blood_scaling: scale how much the brain neuron will cause the blood neuron to attract more blood"

else

	cd heartwork
	pgfortran -traceback -Mcuda flesh.f95 blood.f95 -o megalomaniac_blood
	cd ../neurotic
	pgfortran -traceback -Mcuda discrete_flesh.f95 network.f95 -o megalomaniac_network
	cd ../will
	pgfortran -traceback -Mcuda flesh_out.f95 power.f95 -o megalomaniac_power
	cd ..

	for i in $(seq 1 $3)
	do
		
		
		#operate the heartwork step
		cd heartwork
		#if printer is network only, then dont print the blood
		if [ $9 == "network_only" ]
		then
			./megalomaniac_blood $4 $5 $7 no
		else
			./megalomaniac_blood $4 $5 $7 $9
		fi
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
		if [ -f 'will.txt' ]; then rm will.txt; fi
		cd ..


		#operate the neurotic step
		cd neurotic
		#if printer is network only, then just print the brain
		if [ $9 == "network_only" ]
		then
			./megalomaniac_network $1 $2 $4 $5 9 9 $6 $8 yes
		else
			./megalomaniac_network $1 $2 $4 $5 9 9 $6 $8 $9
		fi
		#if something goes wrong, stop the process
		if [ -f 'neurotic.txt' ] 
		then
			mv neurotic.txt ../will
		else
			cd ..
			echo neurotic copy error
			break
		fi
		#delete recieved networks once finished with
		if [ -f 'heartwork.txt' ]; then rm heartwork.txt; fi
		cd ..

		#operate the will step
		cd will
		./megalomaniac_power $4 $5
		#if something goes wrong, stop the process
		if [ -f 'will.txt' ] 
		then
			mv will.txt ../heartwork
		else
			cd ..
			echo will copy error
			break
		fi
		#delete recieved networks once finished with
		if [ -f 'neurotic.txt' ]; then rm neurotic.txt; fi
		cd ..

	done

	#if [ -f 'heartwork/neurotic.txt' ]; then rm heartwork/neurotic.txt; fi
	#if [ -f 'neurotic/heartwork.txt' ]; then rm neurotic/heartwork.txt; fi
	
fi
