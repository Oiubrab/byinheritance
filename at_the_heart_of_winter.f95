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

!note, need a saved array in txt format to read and input into the vision array



!!!!!!!!!!!!!!!!!!!!
!!!variable setup!!!
!!!!!!!!!!!!!!!!!!!!

!switches and controls
character(len=6) :: output_switch !'motive' for the motivate method and 'normal' for the think method
character(len=5) :: testing_cha,show_blood_cha
logical :: testing=.false., show_blood=.false.,file_exists

!coarray image
integer :: image_total,image_number

!network dimensions
integer :: rows,columns,directions

!input dimension and position
integer :: vision_length,vision_socket
integer,allocatable :: vision(:)

!output dimension and position
integer :: response_length,response_socket
character(len=:),allocatable :: csv_outputter
integer,allocatable :: response(:)

!reward control
real :: node_use_reward
integer,codimension[*] :: oddsey
character(len=20) :: willy

!blood setup
integer :: blood_rows,blood_rate
real :: blood_volume,blood_gradient

!do loop counters
integer :: count_count,column_number


!setup the images
!note: think-1, motivate-2
image_total=num_images()
image_number=this_image()

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











!!!!!!!!!!!!!!!!!!!!!
!!!Setup 1 (think)!!!
!!!!!!!!!!!!!!!!!!!!!
!this setup is for the main brain [image 1]
!this network takes in information, through direct outside interfaces and through motivation
!the response interfaces with the outside 
!socket number represents where the middle of the corresponding array meets the brain network


!allocations
if (image_number==1) then
	!status
	output_switch="normal"
	!dimensions
	!brain
	directions=8; rows=20; columns=50
	vision_length=44
	vision_socket=(columns/2)+1
	response_length=9
	response_socket=(columns/2)+1
	node_use_reward=10.0
	!blood
	blood_rows=rows+1
	blood_rate=20
	blood_volume=8.0
	blood_gradient=0.6
	!allocate the input and output arrays to the dimension fed in to the subroutine
	allocate(vision(vision_length)) !allocate the array for input into the network, currently on top
	allocate(response(response_length)) !allocate the array for output from the network, currently on bottom
end if

!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 1 (think)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!











!!!!!!!!!!!!!!!!!!!!!!!!
!!!Setup 2 (motivate)!!!
!!!!!!!!!!!!!!!!!!!!!!!!
!this setup is for the motivate network [image 2]
!this network takes in vital information
!the response interfaces with neurochem and with think (1)
!socket number represents where the middle of the corresponding array meets the brain network


!allocations
if (image_number==2) then
	!status
	output_switch="motive"
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
	!allocate the input and output arrays to the dimension fed in to the subroutine
	allocate(vision(vision_length)) !allocate the array for input into the network, currently on top
	allocate(response(response_length)) !allocate the array for output from the network, currently on bottom
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 2 (motivate)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!











!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!oddsey reading and feeding!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!the intention here is to calculate some ubiqutous oddsey number from several oddsey numbers across multiple networks
!and reduce them to a single number. Once the number is calculated it is placed in a prime motive image and then populated
!in to all the other image networks. Here, image 2 is the prime motive image 
!first, make sure this isn't the first run
write(willy,"(A4,I0,A4)") "will",image_number,".txt"
INQUIRE(FILE=willy, EXIST=file_exists)
if ((output_switch=="motive") .and. (file_exists .eqv. .true.)) then
	!erect the willy
	open(unit=1,file=willy)
	!do loop skips the brain part and finds the oddsey
	!note, oddsey is line seven so the first six lines must be skipped
	do count_count=1,6
		read(1,*)
	end do
	!read the oddsey number
	read(1,*) oddsey
end if

!send the oddsey number to the other networks
!Again, image 2 is the prime motive image
oddsey=oddsey[2]
	

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end oddsey reading and feeding!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!












!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Input/output/interput structure!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!motivate(2) opens feel and puts it into it's vision
if (image_number==2) then
	open(unit=1,file="world_in_a_world/feel.csv")
	read(1,*) vision	
	close(1)
!think(1) opens sight and feel_response and puts them into it's vision
else if (image_number==1) then
	open(unit=1,file="world_in_a_world/sight.csv")
	open(unit=2,file="world_in_a_world/feel_response.csv")
	!print*,vision_length-response_length[2]-1,vision_length-response_length[2]
	read(1,*) vision(1:vision_length-response_length)
	!print*,vision,vision_length,response_length,size(vision(1:vision_length-response_length)),&
	!	size(vision(vision_length-response_length+1:vision_length))
	read(2,*) vision(vision_length-response_length+1:vision_length-2)		
	close(1)
	close(2)
end if


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end Input/output/interput structure!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!








!!!!!!!!!!!!!!!!!!
!!!run networks!!!
!!!!!!!!!!!!!!!!!!

call insanitorium_deluxe(oddsey,image_number,image_total,rows,columns,directions,node_use_reward,&
	vision,response,vision_socket,response_socket,show_blood,testing,blood_rate,blood_rows,&
	blood_volume,blood_gradient,output_switch)

!!!!!!!!!!!!!!!!!!!!!!
!!!end run networks!!!
!!!!!!!!!!!!!!!!!!!!!!










!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Output and inter-put !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!place response array into a csv file (sight_response.csv)
if (image_number==1) then
	open(unit=1,file="world_in_a_world/sight_response.csv")
!place response motivate array into a csv file (feel_response.csv)
else if (image_number==2) then
	open(unit=1,file="world_in_a_world/feel_response.csv")
end if

!take the arrays and put them into the csv file
allocate(character(2*response_length) :: csv_outputter)
!write each element of the csv file independantly
do column_number=1,response_length-1
	write(csv_outputter((column_number*2)-1:column_number*2),'(I1,A1)') response(column_number),','
end do
write(csv_outputter((response_length*2)-1:response_length*2),'(I1)') response(response_length)
write(1,*) csv_outputter
!print*,response_motivate, response_length
close(1)
	



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! end Output and inter-put !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






sync all

end program
