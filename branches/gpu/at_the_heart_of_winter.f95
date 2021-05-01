program at_the_heart_of_winter
use in_search_of_sanity
use welcome_to_dying
use spiritechnology
implicit none

!this is the program that splits into various images, initialises the network and runs the brain

!this is the main controlling script, where each network is setup and run.
!This code has been written to fit with the nvfortran Nvidia fortran compiler
!this can be found at https://developer.nvidia.com/hpc-sdk and is actually very good

!I take no responsibility for the death and destruction of humankind
!but I do take responsibility for everything else
!I'm sorry. I'm trying to fix it

!Anyway, this is the nvfortran version. 
!To go see the coarrays monster, go checkout the trunk folder



!!!!!!!!!!!!!!!!!!!!
!!!variable setup!!!
!!!!!!!!!!!!!!!!!!!!

!switches and controls
character(len=6) :: output_switch !'motive' for the motivate method and 'normal' for the think method

!cuda image
integer,parameter :: image_total=3
integer :: image_number

!identity image
integer :: ident_number
integer,parameter :: ident_total=5

!network and input output
type(mind),dimension(ident_total) :: think
type(see_say),dimension(ident_total) :: sense

!network read/write
character(len=20) :: will_file
logical :: file_exists

!network dimensions
integer,dimension(ident_total) :: rows,columns
integer,parameter :: directions=8,neuro_history=10

!input dimension and position
integer,dimension(ident_total) :: vision_length
integer,dimension(ident_total) :: vision_socket

!output dimension and position
integer,dimension(ident_total) :: response_length
integer,dimension(ident_total) :: response_socket
character(len=:),allocatable :: csv_outputter

!reward control
real,dimension(ident_total) :: node_use_reward
integer :: oddsey

!blood setup
integer,dimension(ident_total) :: blood_rows
integer,parameter :: blood_rate=20
real,parameter :: blood_volume=0.8,blood_gradient=0.6

!do loop counters
integer :: count_count,column_number

!cuda randomisation initialisation
real :: rander
integer,dimension(ident_total) :: ranseed

!accelerator subroutine and function declarations
!$acc routine(insanitorium_deluxe) seq
!$acc routine(spiritech) seq
!$acc routine(raining_blood) seq
!$acc routine(randomised_list) seq
!$acc routine(random_something) seq
!$acc routine(sigmoid) seq
!$acc routine(position_to_percentage) seq
!$acc routine(binary_to_decimal) seq
!$acc routine(point_to_neuron) seq
!$acc routine(point_origin) seq
!$acc routine(weight_direction) seq
!$acc routine(plugin) seq
!$acc routine(selector) seq
!$acc routine(blood_mover) seq
!$acc routine(weight_direction) seq
!$acc routine(initialiser) seq
!$acc routine(animus) seq
!$acc routine(weight_reducer) seq

!initialise the seed
call random_seed()
do ident_number=1,ident_total
	call random_number(rander)
	ranseed(ident_number)=int(rander*1000.0)
end do

!!!!!!!!!!!!!!!!!!!!!!!!
!!!end variable setup!!!
!!!!!!!!!!!!!!!!!!!!!!!!










!!!!!!!!!!!!!!!!!!!!!!!!
!!!Setup 1 (motivate)!!!
!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the motivate network [image 1, ident 1]
!this network takes in vital information
!the response interfaces with neurochem and with think (1)
!socket number represents where the middle of the corresponding array meets the brain network


!allocations


ident_number=1
!dimensions
!brain
rows(ident_number)=6; columns(ident_number)=17
vision_socket(ident_number)=(columns(ident_number)/2)+1
response_socket(ident_number)=8
node_use_reward(ident_number)=2.0
!blood
blood_rows(ident_number)=rows(ident_number)+1
!allocate the input and output arrays to the dimension fed in to the subroutine
vision_length(ident_number)=15
response_length(ident_number)=7


!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 1 (motivate)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!





!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Setup 1 (sight lust)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the input lust [image 1, ident 2]
!this network takes in information, through direct outside interfaces 
!the response interfaces with the think network
!socket number represents where the middle of the corresponding array meets the brain network


!allocations

!status
output_switch="normal"
ident_number=2
!dimensions
!brain
rows(ident_number)=21; columns(ident_number)=37
vision_socket(ident_number)=(columns(ident_number)/2)+1
response_socket(ident_number)=(columns(ident_number)/2)+1
node_use_reward(ident_number)=10.0
!blood
blood_rows(ident_number)=rows(ident_number)+1
!allocate the input and output arrays to the dimension fed in to the subroutine
vision_length(ident_number)=27
response_length(ident_number)=11


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 1 (sight lust)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!







!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Setup 2 (sight joy)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the input joy [image 2, ident 3]
!this network takes in information, through direct outside interfaces
!the response interfaces with the think network
!socket number represents where the middle of the corresponding array meets the brain network


!allocations
!status
output_switch="normal"
ident_number=3
!dimensions
!brain
rows(ident_number)=21; columns(ident_number)=37
vision_socket(ident_number)=(columns(ident_number)/2)+1
response_socket(ident_number)=(columns(ident_number)/2)+1
node_use_reward(ident_number)=10.0
!blood
blood_rows(ident_number)=rows(ident_number)+1
!allocate the input and output arrays to the dimension fed in to the subroutine
vision_length(ident_number)=27
response_length(ident_number)=11

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 2 (sight joy)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!









!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Setup 3 (sight account)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the input of account data [image 3, ident 4]
!this network takes in information, through direct outside interfaces
!the response interfaces with the think network
!socket number represents where the middle of the corresponding array meets the brain network


!allocations

!status
output_switch="normal"
ident_number=4
!dimensions
!brain
rows(ident_number)=18; columns(ident_number)=31
vision_socket(ident_number)=(columns(ident_number)/2)+6
response_socket(ident_number)=(columns(ident_number)/2)+1
node_use_reward(ident_number)=10.0
!blood
blood_rows(ident_number)=rows(ident_number)+1
!allocate the input and output arrays to the dimension fed in to the subroutine
vision_length(ident_number)=27
response_length(ident_number)=11

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 3 (sight joy)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




!!!!!!!!!!!!!!!!!!!!!
!!!Setup 1 (think)!!!
!!!!!!!!!!!!!!!!!!!!!
!this setup is for the main brain [image 1]
!this network takes in information, from other networks, and compiles a response
!the response interfaces with the outside 
!socket number represents where the middle of the corresponding array meets the brain network


!allocations

!status
output_switch="normal"
ident_number=5
!dimensions
!brain
rows(ident_number)=25; columns(ident_number)=51
vision_socket(ident_number)=(columns(ident_number)/2)+1
response_socket(ident_number)=(columns(ident_number)/2)+1
node_use_reward(ident_number)=10.0
!blood
blood_rows(ident_number)=rows(ident_number)+1
!allocate the input and output arrays to the dimension fed in to the subroutine
vision_length(ident_number)=40
response_length(ident_number)=11

!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 1 (think)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!



!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Network construction!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!


do ident_number=1,ident_total

	!allocate the network to the dimensions fed into the subroutione
	allocate(think(ident_number)%brain_status(2,columns(ident_number),rows(ident_number))) !allocate the brain data and direction status, 1 is for the origin direction of data, 2 is for data status
	allocate(think(ident_number)%brain_weight(directions,directions,columns(ident_number),rows(ident_number))) !allocate the brain direction weighting. 8,:,:,: holds the weights from origin :,x,:,: to point x,:,:,:
	allocate(think(ident_number)%blood(columns(ident_number),blood_rows(ident_number))) !allocate the gradient variable, extra row for response array
	allocate(think(ident_number)%neurochem(2,neuro_history,columns(ident_number),rows(ident_number))) !allocate the reward variable, 1,:,:,: is for origin, 2,:,:,: is for point. :,10,:,: is the weight ladder
	!allocate the input and output arrays to the dimension fed in to the subroutine
	allocate(sense(ident_number)%vision(vision_length(ident_number))) !allocate the vision, to be fed into the network
	allocate(sense(ident_number)%response(response_length(ident_number))) !allocate the response, to be read out from the network



	!this is it
	write(will_file,"(A4,I0,A4)") "will",ident_number,".txt"
	
	
	!if this is is a continuation of the algorithm, then load the previous cycle
	INQUIRE(FILE=will_file, EXIST=file_exists)
	!file_exists=.false.
	if (file_exists .eqv. .true.) then
		!open the previous network
		call read_write(ident_number,ident_total,think(ident_number),"read")
	!Otherwise, if this is the first time this network is activated, it has to be initialised
	else
		
		!initialise the network
		!for think (1)
		call initialiser(think(ident_number),blood_volume,response_socket(ident_number),response_length(ident_number))

	end if


end do





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end network construction!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

















































!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!
!!!Motivation Step!!!
!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!


ident_number=1
!status
output_switch="motive"





















!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!motivate communication structure construct!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!put the array from feel into the vision array
open(unit=1,file="world_in_a_world/feel.csv")
read(1,*) sense(ident_number)%vision
close(1)
	


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end motivate communication structure construct!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!run motivate networks!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!


call insanitorium_deluxe(think(ident_number),oddsey,ident_number,rows(ident_number),&
	columns(ident_number),node_use_reward(ident_number),&
	sense(ident_number)%vision,sense(ident_number)%response,vision_socket(ident_number),&
	response_socket(ident_number),blood_rate,&
	blood_rows(ident_number),blood_volume,blood_gradient,output_switch,ranseed(ident_number))


!calculate the oddsey number
oddsey=oddsey


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end run motivate networks!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!







!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!
!!!End Motivation Step!!!
!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!






























!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!
!!!Sight Step!!!
!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!



!status
output_switch="normal"

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Vision and response preliminary setup!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!place the sight lust array into vision for the sight lust network
open(unit=1,file="world_in_a_world/sight_lust.csv")
read(1,*) sense(2)%vision
!place the sight lust array into vision for the sight lust network
open(unit=2,file="world_in_a_world/sight_joys.csv")
read(2,*) sense(3)%vision	
!place the sight lust array into vision for the sight lust network
open(unit=3,file="world_in_a_world/sight_account.csv")
read(3,*) sense(4)%vision
close(1); close(2); close(3)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end Vision and response preliminary setup!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



do concurrent (image_number=1 : image_total)







	!!!!!!!!!!!!!!!!!!!!!!!!
	!!!run sight networks!!!
	!!!!!!!!!!!!!!!!!!!!!!!!


	call insanitorium_deluxe(think(image_number+ident_number),oddsey,image_number+ident_number,&
		rows(image_number+ident_number),columns(image_number+ident_number),&
		node_use_reward(image_number+ident_number),sense(image_number+ident_number)%vision,&
		sense(image_number+ident_number)%response,vision_socket(image_number+ident_number),&
		response_socket(image_number+ident_number),blood_rate,blood_rows(image_number+ident_number),&
		blood_volume,blood_gradient,output_switch,ranseed(image_number+ident_number))
		



	!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!end run sight networks!!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!


end do


ident_number=image_total+ident_number

!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!
!!!End Sight Step!!!
!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!



























!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!
!!!Think Step!!!
!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!





ident_number=ident_number+1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Vision and response preliminary setup!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end Vision and response preliminary setup!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





!add response from motivate
sense(ident_number)%vision(1:size(sense(1)%response))=sense(1)%response
!add response from sight lust
sense(ident_number)%vision(size(sense(1)%response)+1:size(sense(1)%response)+size(sense(2)%response))=sense(2)%response
!add response from sight joys 
sense(ident_number)%vision(size(sense(1)%response)+size(sense(2)%response)+1:size(sense(1)%response)+&
	size(sense(2)%response)+size(sense(3)%response))=sense(3)%response
!add response from sight account
sense(ident_number)%vision(size(sense(1)%response)+size(sense(2)%response)+size(sense(3)%response)+1:)=sense(4)%response









!!!!!!!!!!!!!!!!!!!!!!!
!!!run think network!!!
!!!!!!!!!!!!!!!!!!!!!!!

!note: here, the think network is run on image one but is given the label of image 4 (image total)
!this is done in order to reuse images, freeing up computing spaces for future activities
call insanitorium_deluxe(think(ident_number),oddsey,ident_number,rows(ident_number),&
	columns(ident_number),node_use_reward(ident_number),&
	sense(ident_number)%vision,sense(ident_number)%response,vision_socket(ident_number),&
	response_socket(ident_number),blood_rate,blood_rows(ident_number),&
	blood_volume,blood_gradient,output_switch,ranseed(ident_number))


!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end run think network!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!






!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!
!!!End Think Step!!!
!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!




























!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Output and inter-put !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!open csv files for output saving
!place response array into a csv file (sight_response.csv)

open(unit=1,file="world_in_a_world/sight_response.csv")

!take the arrays and put them into the csv file
allocate(character(2*response_length(ident_number)) :: csv_outputter)
!write each element of the csv file independantly
do column_number=1,response_length(ident_number)-1
	write(csv_outputter((column_number*2)-1:column_number*2),'(I1,A1)') sense(ident_number)%response(column_number),','
end do
write(csv_outputter((response_length(ident_number)*2)-1:response_length(ident_number)*2),'(I1)') &
	sense(ident_number)%response(response_length)
write(1,*) csv_outputter
!print*,response_motivate, response_length
close(1)
	

!place all the information from the networks in a text file
do ident_number=1,ident_total
	call read_write(ident_number,ident_total,think(ident_number),"write")
end do



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! end Output and inter-put !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






end program
