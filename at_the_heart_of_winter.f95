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


!network dimensions
!stage 1 is motivation, stage 2 is sight and stage 3 is thinking and response
integer,parameter :: directions=8,stages=3
integer,dimension(stages) :: rows,columns

!switches and controls
character(len=6),dimension(stages) :: output_switch !'motive' for the motivate method and 'normal' for the think method
integer,parameter :: epoch_cutoff=2000

!coarray image
integer :: image_total,image_number

!identity image
integer,dimension(stages) :: ident_number
integer :: ident_total

!input/output dimension and position
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

!import the tester
call get_command_argument(1,tester)

!setup the images
image_total=num_images()
image_number=this_image()
!setup the identity total
ident_total=7

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
!!!Setup 1 (money motivate)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the motivate network [image 1, ident 1]
!this network takes in money made over the last ten steps
!the response interfaces with neurochem and with think (1)
!socket number represents where the middle of the corresponding array meets the brain network


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



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Setup 2 (error motivate)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the motivate network [image 2, ident 2]
!this network takes in the amount of invalid trades attempted by the network
!the response interfaces with neurochem and with think (1)
!socket number represents where the middle of the corresponding array meets the brain network


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
!!!Setup 1 (sight lust)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the input lust [image 1, ident 2]
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 2 (sight joy)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!









!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Setup 3 (sight hope)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the input hope [image 2, ident 3]


!this network takes in information, through direct outside interfaces
!the response interfaces with the think network
!socket number represents where the middle of the corresponding array meets the brain network


!allocations
if (image_number==3) then
	!status
	output_switch(count_count)="normal"
	ident_number(count_count)=5

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
!!!end setup 3 (sight hope)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Setup 4 (sight account)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the input of account data [image 3, ident 4]
!this network takes in information, through direct outside interfaces
!the response interfaces with the think network
!socket number represents where the middle of the corresponding array meets the brain network


!allocations
if (image_number==4) then
	!status
	output_switch(count_count)="normal"
	ident_number(count_count)=6
	!dimensions
	!brain
	rows(count_count)=18; columns(count_count)=31
	vision_length(count_count)=27
	vision_socket(count_count)=(columns(count_count)/2)+1
	response_length(count_count)=9
	response_socket(count_count)=(columns(count_count)/2)+1
	node_use_reward(count_count)=10.0
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 4 (sight account)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





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
	ident_number(count_count)=7
	!dimensions
	!brain
	rows(count_count)=35; columns(count_count)=61
	vision_length(count_count)=55
	vision_socket(count_count)=(columns(count_count)/2)+1
	response_length(count_count)=23
	response_socket(count_count)=(columns(count_count)/2)-10
	node_use_reward(count_count)=10.0
end if

!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 1 (think)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!




!!!!!!!!!!!!!!!!!!!!!
!!!end think setup!!!
!!!!!!!!!!!!!!!!!!!!!


sync all

do count_count=1,stages
	!allocate the vision and response
	allocate(trans(count_count)%vision(vision_length(count_count)))
	allocate(trans(count_count)%response(response_length(count_count)))
	sync all
end do

sync all



!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!
!!!End Network setup!!!
!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!
















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
	open(unit=1,file="world_in_a_world/feel.csv")
	read(1,*) trans(stage_count)%vision
	close(1)
	
else if (image_number==2) then
	
	!put the array from errors into the vision array
	open(unit=1,file="world_in_a_world/errors.csv")
	read(1,*) trans(stage_count)%vision
	close(1)
		
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end motivate communication structure construct!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!if (image_number==4) then
	print*,"fuck",image_number
!end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!run motivate networks!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!

if (output_switch(stage_count)=="motive") then
	call insanitorium_deluxe(oddsey,ident_number(stage_count),&
		ident_total,rows(stage_count),columns(stage_count),directions,node_use_reward(stage_count),&
		trans(stage_count)%vision,trans(stage_count)%response,&
		vision_socket(stage_count),response_socket(stage_count),blood_rate,&
		blood_volume,blood_gradient,neurodepth,epoch_cutoff,output_switch(stage_count),tester)
end if

!first, calculate the prime oddsey number from the oddsey numbers outputted	
!send the oddsey number to the other networks
sync all

oddsey=(10*oddsey[1]+oddsey[2])*1000



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
	open(unit=1,file="world_in_a_world/sight_SE1.csv")
	read(1,*) trans(stage_count)%vision
	
!sight joys(2) opens the sight joys csv and puts it into it's vision
else if (image_number==2) then

	!place the sight joys array into vision for the sight joys network
	open(unit=1,file="world_in_a_world/sight_ADV.csv")
	read(1,*) trans(stage_count)%vision
	
!sight hope(3) opens the sight joys csv and puts it into it's vision
else if (image_number==3) then

	!place the sight hope array into vision for the sight joys network
	open(unit=1,file="world_in_a_world/sight_SBR.csv")

	read(1,*) trans(stage_count)%vision
	
!sight account(4) opens the sight account csv and puts it into it's vision
else if (image_number==4) then

	!place the sight account array into vision for the sight joys network
	open(unit=1,file="world_in_a_world/sight_account.csv")
	read(1,*) trans(stage_count)%vision
	
end if


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end Input and motivate communication structure construct!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!











if (image_number==4) then
	print*,"fuck",image_number
end if


!!!!!!!!!!!!!!!!!!!!!!!!
!!!run sight networks!!!
!!!!!!!!!!!!!!!!!!!!!!!!


call insanitorium_deluxe(oddsey,ident_number(stage_count),ident_total,&
	rows(stage_count),columns(stage_count),directions,node_use_reward(stage_count),&
	trans(stage_count)%vision,trans(stage_count)%response,&
	vision_socket(stage_count),response_socket(stage_count),blood_rate,&
	blood_volume,blood_gradient,neurodepth,epoch_cutoff,output_switch(stage_count),tester)
	
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
		!add response from sight lust
		if (count_count<=response_length(2)[1]) then
			trans(stage_count)%vision(count_count)=trans(2)[1]%response(count_count)
		!add response from sight joys
		else if (count_count<=response_length(2)[1]+response_length(2)[2]) then
			trans(stage_count)%vision(count_count)=trans(2)[2]%response(count_count-response_length(2)[1])
		!add response from sight hope
		else if (count_count<=response_length(2)[1]+response_length(2)[2]+response_length(2)[3]) then
			trans(stage_count)%vision(count_count)=trans(2)[3]%response(count_count-&
				(response_length(2)[1]+response_length(2)[2]))
		!add response from sight account		
		else if (count_count<=response_length(2)[1]+response_length(2)[2]+response_length(2)[3]+&
			response_length(2)[4]) then
			trans(stage_count)%vision(count_count)=trans(2)[3]%response(count_count-&
				(response_length(2)[1]+response_length(2)[2]+response_length(2)[3]))
		!add response from motivate feel
		else if (count_count<=response_length(2)[1]+response_length(2)[2]+response_length(2)[3]+&
			response_length(2)[4]+response_length(1)[1]) then
			trans(stage_count)%vision(count_count)=trans(1)[1]%response(count_count-&
				(response_length(2)[1]+response_length(2)[2]+response_length(2)[3]+response_length(2)[4]))
		!add response from motivate errors
		else
			trans(stage_count)%vision(count_count)=trans(1)[2]%response(count_count-&
				(response_length(2)[1]+response_length(2)[2]+response_length(2)[3]+response_length(2)[4]+&
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
	call insanitorium_deluxe(oddsey,ident_number(stage_count),&
		ident_total,rows(stage_count),columns(stage_count),directions,node_use_reward(stage_count),&
		trans(stage_count)%vision,trans(stage_count)%response,&
		vision_socket(stage_count),response_socket(stage_count),blood_rate,&
		blood_volume,blood_gradient,neurodepth,epoch_cutoff,output_switch(stage_count),tester)
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end run think network!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!


if (image_number==4) then
	print*,"you",image_number
end if








!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Output and inter-put !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!open csv files for output saving
!place response array into a csv file (sight_response.csv)
if (image_number==1) then
	open(unit=1,file="world_in_a_world/sight_response.csv")

	!take the arrays and put them into the csv file
	allocate(character(2*response_length(stage_count)) :: csv_outputter)
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





!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!
!!!End Think Step!!!
!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!




sync all

end program
