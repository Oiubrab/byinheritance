program at_the_heart_of_winter
use in_search_of_sanity
use welcome_to_dying
use spiritechnology
implicit none

!this is the program that splits into various images, initialises the network and runs the brain

!this is the main controlling script, where each network is setup and run.
!This code has been written to fit with the opencoarrays 2.9.2 software package
!this can be found at http://www.opencoarrays.org/ and is actually very good

!I take no responsibility for the death and destruction of humankind
!but I do take responsibility for everything else
!I'm sorry. I'm trying to fix it

!Anyway, this is the opencoarrays version. 
!To go see the cuda monster, go checkout the gpu branch



!!!!!!!!!!!!!!!!!!!!
!!!variable setup!!!
!!!!!!!!!!!!!!!!!!!!


!network
!stage 1 is motivation, stage 2 is sight and stage 3 is thinking and response
integer,parameter :: directions=8,stages=3
integer,dimension(stages) :: rows,columns
type(mind),dimension(stages) :: mission
logical :: initialise

!switches and controls
character(len=6),dimension(stages) :: output_switch !'motive' for the motivate method and 'normal' for the think method
integer,parameter :: epoch_cutoff=2000
logical :: stopper

!coarray image
integer :: image_total,image_number

!identity image
integer,dimension(stages) :: ident_number
integer :: ident_total

!input/output
type(see_saw),dimension(stages) :: trans[*]
integer,dimension(stages),codimension[*] :: vision_length=2,response_length=2
integer,dimension(stages) :: vision_socket,response_socket
character(len=:),allocatable :: csv_outputter

!reward control
real,dimension(stages) :: node_use_reward
integer,codimension[*] :: oddsey
integer,parameter :: neurodepth=10

!blood setup
integer,parameter :: blood_rate=20
real,parameter :: blood_volume=0.8,blood_gradient=0.6

!do loop counters
integer :: count_count,column_number,stage_count

!testing
character(len=6) :: tester
integer :: testicle,testicles

!import the tester
call get_command_argument(1,tester)

!setup the images
image_total=num_images()
image_number=this_image()
!setup the identity total
ident_total=5

!setup default values
rows=1 
columns=1
vision_length=1
vision_socket=1
response_length=1
response_socket=1

!!!!!!!!!!!!!!!!!!!!!!!!
!!!end variable setup!!!
!!!!!!!!!!!!!!!!!!!!!!!!





















!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!
!!!Network setup!!!
!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!






!!!!!!!!!!!!!!!!!!!!!!
!!!Motivation setup!!!
!!!!!!!!!!!!!!!!!!!!!!

count_count=1



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Setup 1 (sight motivate)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the motivate network [image 1, ident 1]
!this network takes in the aggregate of the two food positions
!the response interfaces with neurochem and with think (1)
!socket number represents where the middle of the corresponding array meets the network


!allocations
if (image_number==1) then
	!status
	output_switch(count_count)="motive"
	ident_number(count_count)=1
	!dimensions
	!brain
	rows(count_count)=6; columns(count_count)=17
	vision_length(count_count)=15
	vision_socket(count_count)=(columns(count_count)/2)+1
	response_length(count_count)=7
	response_socket(count_count)=9
	node_use_reward(count_count)=2.0
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 1 (money motivate)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Setup 2 (food motivate)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the motivate network [image 2, ident 2]
!this network takes in the food value
!the response interfaces with neurochem and with think (1)
!socket number represents where the middle of the corresponding array meets the network


!allocations
if (image_number==2) then
	!status
	output_switch(count_count)="motive"
	ident_number(count_count)=2
	!dimensions
	!brain
	rows(count_count)=6; columns(count_count)=11
	vision_length(count_count)=5
	vision_socket(count_count)=(columns(count_count)/2)+1
	response_length(count_count)=5
	response_socket(count_count)=6
	node_use_reward(count_count)=3.0
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 2 (error motivate)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!End motivation setup!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!


sync all


!!!!!!!!!!!!!!!!!
!!!sight setup!!!
!!!!!!!!!!!!!!!!!


count_count=2


!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Setup 1 (sight left)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the input left [image 1, ident 3]
!this network takes in information, through direct outside interfaces 
!the response interfaces with the think network
!socket number represents where the middle of the corresponding array meets the brain network


!allocations
if (image_number==1) then
	!status
	output_switch(count_count)="normal"
	ident_number(count_count)=3
	!dimensions
	!brain
	rows(count_count)=21; columns(count_count)=49
	vision_length(count_count)=45
	vision_socket(count_count)=(columns(count_count)/2)+1
	response_length(count_count)=11
	response_socket(count_count)=(columns(count_count)/2)+1
	node_use_reward(count_count)=10.0
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 1 (sight left)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!







!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Setup 2 (sight right)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the input right [image 2, ident 4]
!this network takes in information, through direct outside interfaces
!the response interfaces with the think network
!socket number represents where the middle of the corresponding array meets the brain network


!allocations
if (image_number==2) then
	!status
	output_switch(count_count)="normal"
	ident_number(count_count)=4
	!dimensions
	!brain

	rows(count_count)=21; columns(count_count)=49
	vision_length(count_count)=45
	vision_socket(count_count)=(columns(count_count)/2)+1
	response_length(count_count)=11
	response_socket(count_count)=(columns(count_count)/2)+1
	node_use_reward(count_count)=10.0
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 2 (sight right)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!








!!!!!!!!!!!!!!!!!!!!!
!!!end sight setup!!!
!!!!!!!!!!!!!!!!!!!!!



sync all


!!!!!!!!!!!!!!!!!
!!!think setup!!!
!!!!!!!!!!!!!!!!!

count_count=3

!!!!!!!!!!!!!!!!!!!!!
!!!Setup 1 (think)!!!
!!!!!!!!!!!!!!!!!!!!!
!this setup is for the main brain [image 1]
!this network takes in information, from other networks, and compiles a response
!the response interfaces with the outside 
!socket number represents where the middle of the corresponding array meets the brain network


!allocations
if (image_number==1) then
	!status
	output_switch(count_count)="normal"
	ident_number(count_count)=5
	!dimensions
	!brain
	rows(count_count)=20; columns(count_count)=45
	vision_length(count_count)=35
	vision_socket(count_count)=(columns(count_count)/2)+1
	response_length(count_count)=23
	response_socket(count_count)=(columns(count_count)/2)-1
	node_use_reward(count_count)=10.0
end if

!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 1 (think)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!




!!!!!!!!!!!!!!!!!!!!!
!!!end think setup!!!
!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!
!! Allocation Step !!
!!!!!!!!!!!!!!!!!!!!!

sync all

do count_count=1,stages
	!allocate the vision and response
	allocate(trans(count_count)%vision(vision_length(count_count)))
	allocate(trans(count_count)%response(response_length(count_count)))
	!allocate the mission
	allocate(mission(count_count)%brain_status(2,columns(count_count),rows(count_count)))
	allocate(mission(count_count)%brain_weight(8,8,columns(count_count),rows(count_count)))
	allocate(mission(count_count)%blood(columns(count_count),rows(count_count)+1))
	allocate(mission(count_count)%neurochem(2,neurodepth,columns(count_count),rows(count_count)))
	sync all
end do

if (image_number==1) allocate(character(2*response_length(3)) :: csv_outputter)

sync all


!!!!!!!!!!!!!!!!!!!!!!!!!
!! End Allocation Step !!
!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!
!!!End Network setup!!!
!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!














!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!This is the new song!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!



initialise=.true.
stopper=.false.
do while (stopper .eqv. .false.)

	inquire(file="thing.txt", exist=stopper)

	!!!!!!!!!!!!!!!!!!!!!
	!!!!!!!!!!!!!!!!!!!!!
	!!!Motivation Step!!!
	!!!!!!!!!!!!!!!!!!!!!
	!!!!!!!!!!!!!!!!!!!!!

	stage_count=1

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!motivate communication structure construct!!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


	!motivate(1) opens feel and puts it into it's vision
	if (image_number==1) then
		
		!put the array from feel into the vision array
		open(unit=1,file="alison_hell/sight.csv")
		read(1,*) trans(stage_count)%vision
		close(1)
		
	else if (image_number==2) then
		
		!put the array from errors into the vision array
		open(unit=1,file="alison_hell/food.csv")
		read(1,*) trans(stage_count)%vision
		close(1)
			
	end if

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!end motivate communication structure construct!!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!	do testicles=1,3
!		print*,size(mission(testicles)%brain_weight(:,1,1,1)),size(mission(testicles)%brain_weight(1,:,1,1)),&
!		size(mission(testicles)%brain_weight(1,1,:,1)),size(mission(testicles)%brain_weight(1,1,1,:)),&
!		testicles,image_number,"weight"
!		print*,size(mission(testicles)%neurochem(:,1,1,1)),size(mission(testicles)%neurochem(1,:,1,1)),&
!		size(mission(testicles)%neurochem(1,1,:,1)),size(mission(testicles)%neurochem(1,1,1,:)),&
!		testicles,image_number,"neurochem"
!	end do


	!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!run motivate networks!!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!

	if (output_switch(stage_count)=="motive") then
		call insanitorium_deluxe(initialise,mission(stage_count),oddsey,ident_number(stage_count),&
			node_use_reward(stage_count),trans(stage_count)%vision,trans(stage_count)%response,&
			vision_socket(stage_count),response_socket(stage_count),blood_rate,&
			blood_volume,blood_gradient,epoch_cutoff,output_switch(stage_count),tester)
	end if

	!first, calculate the prime oddsey number from the oddsey numbers outputted	
	!send the oddsey number to the other networks
	sync all

	oddsey=(10*oddsey[1]+oddsey[2])*10000



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











	stage_count=2









	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!Input and motivate communication structure construct!!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



	!sight lust(1) opens the sight lust csv and puts it into it's vision
	if (image_number==1) then

		!place the sight lust array into vision for the sight lust network
		open(unit=1,file="alison_hell/sight_left.csv")
		read(1,*) trans(stage_count)%vision
		
	!sight joys(2) opens the sight joys csv and puts it into it's vision
	else if (image_number==2) then

		!place the sight joys array into vision for the sight joys network
		open(unit=1,file="alison_hell/sight_right.csv")
		read(1,*) trans(stage_count)%vision
		
		
	end if


	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!end Input and motivate communication structure construct!!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!














	!!!!!!!!!!!!!!!!!!!!!!!!
	!!!run sight networks!!!
	!!!!!!!!!!!!!!!!!!!!!!!!

	call insanitorium_deluxe(initialise,mission(stage_count),oddsey,ident_number(stage_count),&
		node_use_reward(stage_count),trans(stage_count)%vision,trans(stage_count)%response,&
		vision_socket(stage_count),response_socket(stage_count),blood_rate,&
		blood_volume,blood_gradient,epoch_cutoff,output_switch(stage_count),tester)
		
	sync all

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!end run sight networks!!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!




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







	stage_count=3














	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!place out put from sight networks and motivate network into think!!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	sync all


	!think(1) opens sight and feel_response and puts them into it's vision
	if (image_number==1) then
		
		

		

		!add response from all four networks
		!the minus one here is because the vision array has an extra position added that isn't used, it's just for padding to make it odd
		do count_count=1,vision_length(3)-1
			!add response from sight left
			if (count_count<=response_length(2)[1]) then
				trans(stage_count)%vision(count_count)=trans(2)[1]%response(count_count)
			!add response from sight jright
			else if (count_count<=response_length(2)[1]+response_length(2)[2]) then
				trans(stage_count)%vision(count_count)=trans(2)[2]%response(count_count-response_length(2)[1])
			!add response from sight motivate 
			else if (count_count<=response_length(2)[1]+response_length(2)[2]+&
				response_length(1)[1]) then
				trans(stage_count)%vision(count_count)=trans(1)[1]%response(count_count-&
					(response_length(2)[1]+response_length(2)[2]))
			!add response from food motivate 
			else
				trans(stage_count)%vision(count_count)=trans(1)[2]%response(count_count-&
					(response_length(2)[1]+response_length(2)[2]+&
					response_length(1)[1]))
			end if
		end do


	end if



	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!end place out put from sight networks and motivate network into think!!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!







	!!!!!!!!!!!!!!!!!!!!!!!
	!!!run think network!!!
	!!!!!!!!!!!!!!!!!!!!!!!

	!note: here, the think network is run on image one but is given the label of image 4 (image total)
	!this is done in order to reuse images, freeing up computing spaces for future activities
	if (image_number==1) then
		call insanitorium_deluxe(initialise,mission(stage_count),oddsey,ident_number(stage_count),&
			node_use_reward(stage_count),trans(stage_count)%vision,trans(stage_count)%response,&
			vision_socket(stage_count),response_socket(stage_count),blood_rate,&
			blood_volume,blood_gradient,epoch_cutoff,output_switch(stage_count),tester)
	end if

	!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!!end run think network!!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!









	!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!! Output and inter-put !!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!


	!open csv files for output saving
	!place response array into a csv file (sight_response.csv)
	if (image_number==1) then
		open(unit=1,file="alison_hell/response.csv")

		!take the arrays and put them into the csv file
		!write each element of the csv file independantly
		do column_number=1,response_length(stage_count)-1
			write(csv_outputter((column_number*2)-1:column_number*2),'(I1,A1)') trans(stage_count)%response(column_number),','
		end do
		write(csv_outputter((response_length(stage_count)*2)-1:response_length(stage_count)*2),'(I1)') &
			trans(stage_count)%response(response_length(stage_count))
		write(1,*) csv_outputter
		close(1)
		
	end if
		



	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!! end Output and inter-put !!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


	initialise=.false.
	!print*,"farewell",image_number
	sync all
end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!End this is the new song!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!
!!!End Think Step!!!
!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!


end program
