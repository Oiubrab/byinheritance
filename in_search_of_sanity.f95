program in_search_of_sanity
use welcome_to_dying
use spiritechnology

!this is the main controlling script, where the network is setup and run.

!note, need a saved array in txt format to read and input into the vision array

implicit none

!network setup and reading
integer,codimension[*] :: directions, rows, columns
type(mind) :: think,motivate !think is the vision and response, motivate is the cash flow and neurochem controller
logical :: file_exists
real,codimension[*] :: node_use_reward

!sensing and response setup
!think array interfaces
integer, allocatable :: vision(:), response(:), response_counter(:,:)
!motivate array interfaces
integer, allocatable :: vision_motivate(:), response_motivate(:), response_counter_motivate(:,:)
integer :: oddsey
!ubiquitous
integer :: movement, speed
integer,codimension[*] :: vision_length, response_length
integer,codimension[*] :: vision_socket, response_socket

!selecting and moving
integer :: column_number, column_number_2
integer,codimension[*] :: moves, epoch, epoch_start

!from the outside
character(len=1000) :: show_blood_cha,testing_cha
integer :: epoch_cutoff=100 !ubiqutous

!blood
integer,codimension[*] :: blood_rate
integer,codimension[*] :: blood_rows
real,codimension[*] :: blood_volume, blood_gradient

!testing
real :: random_see
logical :: testing=.false., show_blood=.false.
character(len=:),allocatable :: column_think_cha,column_motivate_cha

!timing
real :: start, finish, delay_time=0.0

!printing


!coarray specific
integer :: image_number,image_total
character(len=20) :: test_file, will_file






!setup the images
!note: think-1, motivate-2
image_total=num_images()
image_number=this_image()
write(test_file,"(A8,I0,A4)") "test_log",image_number,".txt"
write(will_file,"(A4,I0,A4)") "will",image_number,".txt"

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






!!!!!!!!!!!!!!!!!!!!!
!!!Setup 1 (think)!!!
!!!!!!!!!!!!!!!!!!!!!
!this setup is for the main brain [image 1]
!this network takes in information, through direct outside interfaces and through motivation
!the response interfaces with the outside 
!socket number represents where the middle of the corresponding array meets the brain network


!allocations
if (image_number==1) then
	!dimensions
	!brain
	directions[1]=8; rows[1]=50; columns[1]=50
	vision_length[1]=35
	vision_socket[1]=(columns[1]/2)+1
	response_length[1]=7
	response_socket[1]=(columns[1]/2)+1
	node_use_reward[1]=10.0
	!blood
	blood_rows[1]=rows+1
	blood_rate[1]=20
	blood_volume[1]=8.0
	blood_gradient[1]=0.6
	!main brain
	allocate(think%brain_status(2,columns[1],rows[1])) !allocate the brain data and direction status, 1 is for the origin direction of data, 2 is for data status
	allocate(think%brain_weight(directions[1],directions[1],columns[1],rows[1])) !allocate the brain direction weighting. 8,:,:,: holds the weights from origin :,x,:,: to point x,:,:,:
	allocate(think%blood(columns[1],blood_rows[1])) !allocate the gradient variable, extra row for response array
	allocate(think%neurochem(2,10,columns[1],rows[1])) !allocate the reward variable, 1,:,:,: is for origin, 2,:,:,: is for point. :,10,:,: is the weight ladder
	!input-output
	allocate(vision(vision_length[1])) !allocate the array for input into the network, currently on top
	allocate(response(response_length[1])) !allocate the array for output from the network, currently on bottom
	!logging
	allocate(character(columns[1]*3+1) :: column_think_cha) !allocate the printing variable
	allocate(response_counter(response_length[1],vision_length[1])) !allocat the array that will keep track of the response for printing purposes
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
	!dimensions
	!brain
	directions[2]=8; rows[2]=6; columns[2]=15
	vision_length[2]=columns[2]
	vision_socket[2]=(columns[2]/2)+1
	response_length[2]=7
	response_socket[2]=8
	node_use_reward[2]=2.0
	!blood
	blood_rows[2]=rows[2]+1
	blood_rate[2]=20
	blood_volume[2]=8.0
	blood_gradient[2]=0.6
	!main brain
	allocate(motivate%brain_status(2,columns[2],rows[2])) !allocate the brain data and direction status, 1 is for the origin direction of data, 2 is for data status
	allocate(motivate%brain_weight(directions[2],directions[2],columns[2],rows[2])) !allocate the brain direction weighting. 8,:,:,: holds the weights from origin :,x,:,: to point x,:,:,:
	allocate(motivate%blood(columns[2],blood_rows[2])) !allocate the gradient variable, extra row for response array
	allocate(motivate%neurochem(2,10,columns[2],rows[2])) !allocate the reward variable, 1,:,:,: is for origin, 2,:,:,: is for point. :,10,:,: is the timer
	!input-output
	allocate(vision_motivate(vision_length[2])) !allocate the array for input into the network, currently on top
	allocate(response_motivate(response_length[2])) !allocate the array for output from the network, currently on bottom
	!logging
	allocate(character(columns[2]*3+1) :: column_motivate_cha) !allocate the printing variable
	allocate(response_counter_motivate(response_length,vision_length)) !allocat the array that will keep track of the response for printing purposes
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!end setup 2 (motivate)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!












!cutoff unused coarrays
if (image_number<=2) then 

		
	sync all
	!this is it
	
	!start timer
	call CPU_Time(start)

	!fuck you
	call random_seed()
	
	!think(1) opens outside_data and puts it into it's vision
	if (image_number==1) then
		open(unit=1,file="outside_data.csv")
		read(1,*) vision
	else if (image_number==2) then
		open(unit=1,file="inside_data.csv")
		read(1,*) vision_motivate	
	end if
	
	!if this is is a continuation of the algorithm, then load the previous cycle
	INQUIRE(FILE=will_file, EXIST=file_exists)
	!file_exists=.false.
	if (file_exists .eqv. .true.) then
		!zero out the response first
		!for think (1)
		if (image_number==1) then
			response=0
			!open the test log
			if (testing .eqv. .true.) then
				call read_write(think,epoch,moves,"read",response_counter)
			else
				call read_write(think,epoch,moves,"read")
			end if
			
		!for motivate (2)
		else if (image_number==2) then
			response_motivate=0
			!open the test log
			if (testing .eqv. .true.) then
				call read_write(motivate,epoch,moves,"read",response_counter_motivate)
			else
				call read_write(motivate,epoch,moves,"read")
			end if
		end if
		epoch_start=epoch
		
	!Otherwise, if this is the first time this network is activated, it has to be initialised
	else
		
		
		!initialise the network
		!for think (1)
		if (image_number==1) then
			call initialiser(think,response,blood_volume[1],response_socket[1],response_counter)
			!call preprogram(think%brain_weight)
		!for motivate (2)
		else if (image_number==2) then
			call initialiser(motivate,response_motivate,blood_volume[2],response_socket[2],response_counter_motivate)
			!call preprogram(think%brain_weight)
		end if

		

		!inject data from the vission arrays into the networks
		do column_number=1,vision_length[image_number]
			!for think (1)
			if (image_number==1) then
				if (vision(column_number)==1) then	
					think%brain_status(1,plugin(column_number,vision_socket[1],vision_length[1],"brain"),1)=2
					think%brain_status(2,plugin(column_number,vision_socket[1],vision_length[1],"brain"),1)=1
				end if
			!for motivate (2)
			else if (image_number==2) then
				if (vision_motivate(column_number)==1) then
					motivate%brain_status(1,plugin(column_number,vision_socket[2],vision_length[2],"brain"),1)=2
					motivate%brain_status(2,plugin(column_number,vision_socket[2],vision_length[2],"brain"),1)=1
				end if					
			end if
		end do


		epoch=0
		epoch_start=0
		moves=0


		!print the first network for testing purposes
		if (testing .eqv. .true.) then
		
			!read(image_number,*)image_number_cha
			!open the test log
			open(unit=image_number,file=test_file)
			!save the network
			!print*,test_file,will_file
			!print*,vision,"in_search_of_sanity"
			!print*,vision_motivate,"in_search_of_sanity"
			
			write(image_number,*)"By Inheritance"
			write(image_number,*)"Brain moves: 0 Epoch: 0"
			close(image_number)
			
			!for think (1)
			if (image_number==1) then
				if (show_blood .eqv. .true.) then
					call print_network(moves,epoch,vision,vision_socket[1],response,response_socket[1],think%brain_status,think%blood)
				else
					call print_network(moves,epoch,vision,vision_socket[1],response,response_socket[1],think%brain_status)
				end if
			!for motivate (2)
			else if (image_number==2) then
				if (show_blood .eqv. .true.) then
					call print_network(moves,epoch,vision_motivate,vision_socket[2],response_motivate,response_socket[2],&
						motivate%brain_status,motivate%blood)
				else
					call print_network(moves,epoch,vision_motivate,vision_socket[2],response_motivate,response_socket[2],motivate%brain_status)
				end if
			end if

		end if

		

	end if



	!!!!!!!!!!!!
	!!!reward!!!
	!!!!!!!!!!!!
	




	!injection, from vision into brain
	!inject data from the vission arrays into the networks
	do column_number=1,vision_length[image_number]
		!for think (1)
		if (image_number==1) then
			if (vision(column_number)==1) then	
				think%brain_status(1,plugin(column_number,vision_socket[1],vision_length[1],"brain"),1)=2
				think%brain_status(2,plugin(column_number,vision_socket[1],vision_length[1],"brain"),1)=1
			end if
		!for motivate (2)
		else if (image_number==2) then
			if (vision_motivate(column_number)==1) then
				motivate%brain_status(1,plugin(column_number,vision_socket[2],vision_length[2],"brain"),1)=2
				motivate%brain_status(2,plugin(column_number,vision_socket[2],vision_length[2],"brain"),1)=1
			end if					
		end if
	end do


	
	
	
	
	

	!!!!!!!!!!!
	!!! Die !!!
	!!!!!!!!!!!
	!This is the kernal, the grand daddy of this whole rotten affair
	!for think (1)
	if (image_number==1) then
		call spiritech(epoch[1],think,blood_rate[1],response_socket[1],response_length[1],vision_length[1],&
			vision_socket[1],blood_rows[1],epoch_cutoff,blood_gradient[1],blood_volume[1],vision,response,response_counter,&
			rows[1],columns[1],moves[1],testing,show_blood,delay_time,epoch_start[1],node_use_reward[1])
	!for motivate (2)
	else if (image_number==2) then
		call spiritech(epoch[2],motivate,blood_rate[2],response_socket[2],response_length[2],vision_length[2],&
			vision_socket[2],blood_rows[2],epoch_cutoff,blood_gradient[2],blood_volume[2],vision_motivate,response_motivate,&
			response_counter_motivate,rows[2],columns[2],moves[2],testing,show_blood,delay_time,epoch_start[2],node_use_reward[2])
	end if


	

	!!!!!!!!!!!!!!!!!!!!!!!
	!!! Testing Summary !!!
	!!!!!!!!!!!!!!!!!!!!!!!


	!print all the run data
	if (testing .eqv. .true.) then

		!first, open the testing log back up
		open(unit=image_number,file=test_file,access="APPEND")

		!report results of each channel between vision and response at the end
		!first, setup printing format
		!first, for think (1)
		if (image_number==1) then
			column_think_cha(1:1) = "("
			do column_number=1,response_length[image_number]
				if (column_number==response_length[image_number]) then
					column_think_cha(2+3*(column_number-1):2+3*(column_number-1)+2)="I4)"
				else
					column_think_cha(2+3*(column_number-1):2+3*(column_number-1)+2)="I4,"
				end if
			end do
			!now, print each combinating of vision and response
			do column_number=1,vision_length[image_number]
				write(image_number,"(A28,I0,A1)")"Response counter for vision ",column_number,":"
				write(image_number,column_think_cha) response_counter(:,column_number)
			end do
		
		!now, for motivate (2)
		else if (image_number==2) then
			column_motivate_cha(1:1) = "("
			do column_number=1,response_length[image_number]
				if (column_number==response_length[image_number]) then
					column_motivate_cha(2+3*(column_number-1):2+3*(column_number-1)+2)="I4)"
				else
					column_motivate_cha(2+3*(column_number-1):2+3*(column_number-1)+2)="I4,"
				end if
			end do
			!now, print each combinating of vision and response
			do column_number=1,vision_length[image_number]
				write(image_number,"(A28,I0,A1)")"Response counter for vision ",column_number,":"
				write(image_number,column_motivate_cha) response_counter_motivate(:,column_number)
			end do	
		end if	

		!end timer
		call CPU_Time(finish)

		!print time elapsed
		write(image_number,*)" "
		call print_interval_multiple(start,finish)

		!finally, close the file
		close(image_number)

		!add response counter to will
		!for think (1)
		if (image_number==1) then
			call read_write(think,epoch,moves,"write",response_counter)
		!for motivate (2)
		else if (image_number==2) then
			call read_write(motivate,epoch,moves,"write",response_counter_motivate)
		end if

	else

		!place all the information network in a text file
		!for think (1)
		if (image_number==1) then
			call read_write(think,epoch,moves,"write")
		!for motivate (2)
		else if (image_number==2) then
			call read_write(motivate,epoch,moves,"write")
		end if

	end if
	

	!!!!!!!!!!!!!!!!!!!!!!
	!!! Motivation Act !!!
	!!!!!!!!!!!!!!!!!!!!!!
	
	sync all
	
	!oddsey is the multiplier for the neurochem effect, as defined by the motivate network
	!oddsey=motivate%
	
	!this makes the system yearn for happiness
	!call animus(think,motivate,oddsey)
	

end if


end program in_search_of_sanity
