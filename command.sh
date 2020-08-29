#!bin/bash

rm questions
gfortran -g -fcheck=all -Wall -o questions  discrete_flesh.f95  emerge.f95  flesh.f95  flesh_out.f95 answers.f95  questions.f95 
if [ "$#" -eq 2 ]
then
	if [ $1 == "delete" ]
	then
		rm will.txt 
	fi
fi
for i in $(seq 1 $(($2-1)))
do
	./questions 2.0 8 10 101 99 0 0.05 0.1 yes
done
reset 
./questions 2.0 8 10 101 99 0 0.05 0.1 yes
