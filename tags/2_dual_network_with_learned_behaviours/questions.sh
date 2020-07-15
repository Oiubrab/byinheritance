#!bin/bash

#start the timer
start=$(date +%s.%N)

#delete all the old text files and error printouts
if [ -f 'heartwork/will.txt' ]; then rm heartwork/will.txt; fi
if [ -f 'neurotic/heartwork.txt' ]; then rm neurotic/heartwork.txt; fi
if [ -f 'will/neurotic.txt' ]; then rm will/neurotic.txt; fi
rm -r neurotic/error_folder
mkdir neurotic/error_folder

reset

if [ "$#" -ne 9 ]
then
    echo "Execute program by format:"
	echo ". questions.sh valves valve_value cycles maximum_columns maximum_rows lag blood_scaling network_scaling printed"
	echo "valves: left right up down custom"
	echo "printed: yes no debug network_only power_only"
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
		elif [ $9 == "power_only" ]
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
			./megalomaniac_network $1 $2 $4 $5 8 10 $8 yes
		elif [ $9 == "power_only" ]
		then
			./megalomaniac_network $1 $2 $4 $5 8 10 $8 no			
		else
			./megalomaniac_network $1 $2 $4 $5 8 10 $8 $9
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
		if [ $9 == "network_only" ]
		then
			./megalomaniac_power $3 $4 $5 $6 $8 no
		elif [ $9 == "power_only" ]
		then
			./megalomaniac_power $3 $4 $5 $6 $8 yes			
		else
			./megalomaniac_power $3 $4 $5 $6 $8 $9
		fi
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
	
fi

#stop the timer
finish=$(date +%s.%N)

#print the total run time
echo " "
echo "total run time is:"
runtime=`echo "var=sqrt(($finish-$start)^2);var" | bc`
runtime_hour=`echo "var=$runtime/3600;var" | bc`
runtime_minute=`echo "var=($runtime-$runtime_hour*3600)/60;var" | bc`
runtime_second_long=`echo "var=$runtime-$runtime_hour*3600-$runtime_minute*60;var" | bc`
runtime_second_length=${#runtime_second_long}
runtime_second_cut=`echo "var=$runtime_second_length-7;var" | bc`
runtime_second=$(echo $runtime_second_long | cut -c1-$runtime_second_cut)
echo $runtime_hour "hrs, " $runtime_minute "mins, " $runtime_second "sec"
