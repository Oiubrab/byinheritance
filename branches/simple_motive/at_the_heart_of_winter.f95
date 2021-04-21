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
!To go see the cuda monster, go checkout the_years_of_decay.f95

!note, need a saved array in txt format to read and input into the vision arrays



!!!!!!!!!!!!!!!!!!!!
!!!variable setup!!!
!!!!!!!!!!!!!!!!!!!!

!switches and controls
character(len=6) :: output_switch !'motive' for the motivate method and 'normal' for the think method
character(len=5) :: testing_cha,show_blood_cha
logical :: testing=.false., show_blood=.false.,file_exists

!coarray image
integer :: image_total,image_number

!identity image
integer :: ident_number,ident_total

!network dimensions
integer :: rows,columns,directions

!input dimension and position
integer :: vision_length,vision_socket
integer,allocatable :: vision(:)

!output dimension and position
integer :: response_length,response_socket
character(len=:),allocatable :: csv_outputter
integer,allocatable :: response(:)[:]

!reward control
real :: node_use_reward
integer,codimension[*] :: oddsey
character(len=20) :: willy

!blood setup
integer :: blood_rows,blood_rate
real :: blood_volume,blood_gradient

!do loop counters
integer :: count_count,column_number

!motive specific
integer,allocatable :: response_motivate(:)


!setup the images
image_total=num_images()
image_number=this_image()
!setup the identity total
ident_total=4

!!!!!!!!!!!!!!!!!!!!!!!!
!!!end variable setup!!!
!!!!!!!!!!!!!!!!!!!!!!!!













!!!!!!!!!!!!!!!!!!!!!!
!!!testing switches!!!
!!!!!!!!!!!!!!!!!!!!!!

!testing switches
!switch for turning on/off the test logs
CALL GET_COMMAND_ARGUMENT(1,testing_cha)
if (testing_cha=="true") then
	testing=.true.
else if (testing_cha=="false") then
	testing=.false.
ENDIF
!switch for turning on/off blood printing
CALL GET_COMMAND_ARGUMENT(2,show_blood_cha)
if (show_blood_cha=="true") then
	show_blood=.true.
else if (show_blood_cha=="false") then
	show_blood=.false.
ENDIF


!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end testing switches!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!























!!!!!!!!!!!!!!!!!!!!!!!!
!!!Setup 1 (motivate)!!!
!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the motivate network [image 2]
!this network takes in vital information
!the response interfaces with neurochem and with think (1)
!socket number represents where the middle of the corresponding array meets the brain network


!allocations
if (image_number==1) then
	!status
	output_switch="motive"
	ident_number=1
	!dimensions
	!brain
	directions=8; rows=6; columns=15
	vision_length=columns
	vision_socket=(columns/2)+1
	response_length=7
	response_socket=8
	node_use_reward=2.0
	!blood
	blood_rows=rows+1
	blood_rate=20
	blood_volume=8.0
	blood_gradient=0.6
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 1 (motivate)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!














!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!motivate communication structure construct!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!motivate(1) opens feel and puts it into it's vision
if (image_number==1) then

	!allocate the input and output arrays to the dimension fed in to the subroutine
	allocate(vision(vision_length)) !allocate the array for input into the network, currently on top
	allocate(response_motivate(response_length)) !allocate the array for output from the network, currently on bottom
	
	!put the array from feel into the vision array
	open(unit=1,file="world_in_a_world/feel_growth.csv")
	read(1,*) vision	
	close(1)
	
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end motivate communication structure construct!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!run motivate networks!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!

if (output_switch=="motive") then
	call insanitorium_deluxe(oddsey,ident_number,ident_total,rows,columns,directions,node_use_reward,&
		vision,response_motivate,vision_socket,response_socket,show_blood,testing,blood_rate,blood_rows,&
		blood_volume,blood_gradient,output_switch)
end if
	
!send the oddsey number to the other networks
!image 1 is the prime motive image
sync all
oddsey=oddsey[1]

!deallocate the motive vision array
if (image_number==1) then
	deallocate(vision)
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end run motivate networks!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!








!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Setup 1 (sight lust)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the input lust [image 3]
!this network takes in information, through direct outside interfaces 
!the response interfaces with the think network
!socket number represents where the middle of the corresponding array meets the brain network


!allocations
if (image_number==1) then
	!status
	output_switch="normal"
	ident_number=2
	!dimensions
	!brain
	directions=8; rows=18; columns=31
	vision_length=19
	vision_socket=(columns/2)+1
	response_length=11
	response_socket=(columns/2)+1
	node_use_reward=10.0
	!blood
	blood_rows=rows+1
	blood_rate=20
	blood_volume=8.0
	blood_gradient=0.6
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 1 (sight lust)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!







!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Setup 2 (sight joy)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the input joy [image 4]
!this network takes in information, through direct outside interfaces
!the response interfaces with the think network
!socket number represents where the middle of the corresponding array meets the brain network


!allocations
if (image_number==2) then
	!status
	output_switch="normal"
	ident_number=3
	!dimensions
	!brain
	directions=8; rows=18; columns=31
	vision_length=19
	vision_socket=(columns/2)+1
	response_length=11
	response_socket=(columns/2)+1
	node_use_reward=10.0
	!blood
	blood_rows=rows+1
	blood_rate=20
	blood_volume=8.0
	blood_gradient=0.6
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 2 (sight joy)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


















!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Input and motivate communication structure construct!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!allocate the input and output arrays to the dimension fed in to the subroutine
allocate(vision(vision_length)) !allocate the array for input into the network, currently on top
allocate(response(response_length)[*]) !allocate the array for output from the network, currently on bottom


!sight lust(2) opens the sight lust csv and puts it into it's vision
if (image_number==1) then

	!place the sight lust array into vision for the sight lust network
	open(unit=1,file="world_in_a_world/sight_lust.csv")
	read(1,*) vision
	
!sight joys(3) opens the sight joys csv and puts it into it's vision
else if (image_number==2) then

	!place the sight joys array into vision for the sight joys network
	open(unit=1,file="world_in_a_world/sight_joys.csv")
	read(1,*) vision
	
end if


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end Input and motivate communication structure construct!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!














!!!!!!!!!!!!!!!!!!!!!!!!
!!!run sight networks!!!
!!!!!!!!!!!!!!!!!!!!!!!!


call insanitorium_deluxe(oddsey,ident_number,ident_total,rows,columns,directions,node_use_reward,&
	vision,response,vision_socket,response_socket,show_blood,testing,blood_rate,blood_rows,&
	blood_volume,blood_gradient,output_switch)
	

!deallocate the vision array
deallocate(vision)

sync all

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end run sight networks!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!












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
	output_switch="normal"
	ident_number=4
	!dimensions
	!brain
	directions=8; rows=21; columns=41
	vision_length=29
	vision_socket=(columns/2)+1
	response_length=11
	response_socket=(columns/2)+1
	node_use_reward=10.0
	!blood
	blood_rows=rows+1
	blood_rate=20
	blood_volume=8.0
	blood_gradient=0.6
	!allocate the input and output arrays to the dimension fed in to the subroutine
	
	!allocate the array for output from the network, currently on bottom
end if

!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 1 (think)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!











!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!place out put from sight networks and motivate network into think!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!think(1) opens sight and feel_response and puts them into it's vision
if (image_number==1) then

	!allocate the array for input into the network, currently on top
	allocate(vision(vision_length)) 

	!add response from sight lust
	vision(1:size(response(:)[1]))=response(:)[1]
	!add response from sight joys
	vision(size(response(:)[1])+1:size(response(:)[1])+size(response(:)[2]))=response(:)[2]
	!add response from motivate
	vision(size(response(:)[1])+size(response(:)[2])+1:)=response_motivate

end if


!deallocate the response array to be used for think and allocate to think specs
deallocate(response)
allocate(response(response_length)[*]) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end place out put from sight networks and motivate network into think!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!







!!!!!!!!!!!!!!!!!!!!!!!
!!!run think network!!!
!!!!!!!!!!!!!!!!!!!!!!!

!note: here, the think network is run on image one but is given the label of image 4 (image total)
!this is done in order to reuse images, freeing up computing spaces for future activities
if (image_number==1) then
	call insanitorium_deluxe(oddsey,ident_number,ident_total,rows,columns,directions,node_use_reward,&
		vision,response,vision_socket,response_socket,show_blood,testing,blood_rate,blood_rows,&
		blood_volume,blood_gradient,output_switch)
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
	open(unit=1,file="world_in_a_world/sight_response.csv")

	!take the arrays and put them into the csv file
	allocate(character(2*response_length) :: csv_outputter)
	!write each element of the csv file independantly
	do column_number=1,response_length-1
		write(csv_outputter((column_number*2)-1:column_number*2),'(I1,A1)') response(column_number)[image_number],','
	end do
	write(csv_outputter((response_length*2)-1:response_length*2),'(I1)') response(response_length)[image_number]
	write(1,*) csv_outputter
	!print*,response_motivate, response_length
	close(1)
	
end if
	



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! end Output and inter-put !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






sync all

end program
