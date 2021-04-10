module in_search_of_sanity
use welcome_to_dying
use spiritechnology
use reign_in_blood
contains

!this subroutine is the main kernalised routine for brain creation and action
!here, each network is created as a will file (text) and is propogated through
!from one input to one output

!run this script in parallel, with inputs connected to the outputs of other networks for a truly integrated experience

!Note, there are two versons here:
!Motive - input feeds into a single data output
!Normal - input feeds in and output keeps building until the stop output is activated
!output_switcher is the feed in switch that determines whether the network is normal or motive 

subroutine insanitorium_deluxe(oddsey,image_number,image_total,rows,columns,directions,node_use_reward,&
	vision,response,vision_socket,response_socket,show_blood,testing,blood_rate,blood_rows,&
	blood_volume,blood_gradient,output_switcher)

	implicit none

	!network setup and reading
	integer :: rows, columns, directions
	type(mind) :: think !think is the vision and response, motivate is the cash flow and neurochem controller
	logical :: file_exists
	real :: node_use_reward

	!sensing and response setup
	!think array interfaces
	integer,dimension(*) :: vision(:), response(:)
	integer,allocatable :: response_counter(:,:)
	!motivate array interfaces
	integer :: oddsey
	!ubiquitous
	integer :: vision_length, response_length
	integer :: vision_socket, response_socket

	!selecting and moving
	integer :: column_number, column_number_2
	integer :: moves, epoch, epoch_start

	!from the outside
	integer :: epoch_cutoff=100 !ubiqutous

	!blood
	integer :: blood_rate
	integer :: blood_rows
	real :: blood_volume, blood_gradient

	!testing
	character(len=:),allocatable :: column_think_cha
	logical :: show_blood,testing

	!timing
	real :: start, finish, delay_time=0.0

	!output
	character(len=6) :: output_switcher !the switch for normal and motivation networks

	!coarray specific
	integer :: image_number,image_total
	character(len=20) :: test_file, will_file



	!define the size of the vision and response arrays
	vision_length=size(vision); response_length=size(response)


	!allocate the logging variables
	allocate(character(columns*3+1) :: column_think_cha) !allocate the printing variable
	allocate(response_counter(response_length,vision_length)) !allocate the array that will keep track of the response for printing purposes

	!allocate the network to the dimensions fed into the subroutione
	allocate(think%brain_status(2,columns,rows)) !allocate the brain data and direction status, 1 is for the origin direction of data, 2 is for data status
	allocate(think%brain_weight(directions,directions,columns,rows)) !allocate the brain direction weighting. 8,:,:,: holds the weights from origin :,x,:,: to point x,:,:,:
	allocate(think%blood(columns,blood_rows)) !allocate the gradient variable, extra row for response array
	allocate(think%neurochem(2,10,columns,rows)) !allocate the reward variable, 1,:,:,: is for origin, 2,:,:,: is for point. :,10,:,: is the weight ladder







	!this is it

	!cutoff unused coarrays
	if (image_number<=image_total) then 
		
		
		!start timer
		call CPU_Time(start)

		write(test_file,"(A8,I0,A4)") "test_log",image_number,".txt"
		write(will_file,"(A4,I0,A4)") "will",image_number,".txt"

		!fuck you
		call random_seed()
		

		!took sight and feel input from here into "at the heart of winter"
		
		
		!if this is is a continuation of the algorithm, then load the previous cycle
		INQUIRE(FILE=will_file, EXIST=file_exists)
		!file_exists=.false.
		if (file_exists .eqv. .true.) then
			!zero out the response first
			!for think (1)
			response=0
			!open the test log
			if (testing .eqv. .true.) then
				call read_write(image_number,image_total,think,epoch,moves,"read",response_record=response_counter)
			else
				call read_write(image_number,image_total,think,epoch,moves,"read")
			end if
				
			!this makes the system yearn for happiness
			!must be run for each network
			if (output_switcher=="motive") then
				call raining_blood(think,vision,oddsey,image_number)
			end if
			call animus(think,oddsey)

			epoch_start=epoch
			
		!Otherwise, if this is the first time this network is activated, it has to be initialised
		else
			
			
			!initialise the network
			!for think (1)
			call initialiser(think,response,blood_volume,response_socket,response_counter)


			

			!inject data from the vission arrays into the networks
			do column_number=1,vision_length
				if (vision(column_number)==1) then	
					think%brain_status(1,plugin(column_number,vision_socket,vision_length,"brain"),1)=2
					think%brain_status(2,plugin(column_number,vision_socket,vision_length,"brain"),1)=1
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
				
				!either print the network with the corresponding blood network, or don't
				if (show_blood .eqv. .true.) then
					call print_network(moves,epoch,vision,vision_socket,response,response_socket,think%brain_status,think%blood)
				else
					call print_network(moves,epoch,vision,vision_socket,response,response_socket,think%brain_status)
				end if


			end if

			

		end if

		




		!injection, from vision into brain
		!inject data from the vission arrays into the networks
		do column_number=1,vision_length
			if (vision(column_number)==1) then	
				think%brain_status(1,plugin(column_number,vision_socket,vision_length,"brain"),1)=2
				think%brain_status(2,plugin(column_number,vision_socket,vision_length,"brain"),1)=1
			end if
		end do


		
		
		
		
		

		!!!!!!!!!!!
		!!! Die !!!
		!!!!!!!!!!!
		!This is the kernal, the grand daddy of this whole rotten affair
		!this subroutine moves through the network and blood and propogates all data movement
		call spiritech(epoch,think,blood_rate,response_socket,response_length,vision_length,&
			vision_socket,blood_rows,epoch_cutoff,blood_gradient,blood_volume,vision,response,response_counter,&
			rows,columns,moves,testing,show_blood,delay_time,epoch_start,node_use_reward,image_number,output_switcher)





		!!!!!!!!!!!!!!!!!!!!!!!!!!
		!!! Motivation Decider !!!
		!!!!!!!!!!!!!!!!!!!!!!!!!!
		
		
		!oddsey is the multiplier for the neurochem effect, as defined by the motivate network
		!oddsey is defined only in the motivate image

		if (output_switcher=="motive") then
			oddsey=findloc(response,1,dim=1)
			!if no data comes through, don't change the weights
			if (oddsey==0) then
				oddsey=(size(response)/2)
			end if
			!right is higher motivation, left is lower motivation
			!midpoint is zero motivation and anything to the left is negative
			oddsey=oddsey-(size(response)/2)	
		end if

		

		!if (image_number==1) then
		!	print*,response
		!end if
		

		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		!!! Testing Summary and/or network save !!!
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


		!print all the run data
		if (testing .eqv. .true.) then

			!first, open the testing log back up
			open(unit=image_number,file=test_file,access="APPEND")

			!report results of each channel between vision and response at the end
			!first, setup printing format

			column_think_cha(1:1) = "("
			do column_number=1,response_length
				if (column_number==response_length) then
					column_think_cha(2+3*(column_number-1):2+3*(column_number-1)+2)="I4)"
				else
					column_think_cha(2+3*(column_number-1):2+3*(column_number-1)+2)="I4,"
				end if
			end do
			!now, print each combinating of vision and response
			do column_number=1,vision_length
				write(image_number,"(A28,I0,A1)")"Response counter for vision ",column_number,":"
				write(image_number,column_think_cha) response_counter(:,column_number)
			end do

			!end timer
			call CPU_Time(finish)

			!print time elapsed
			write(image_number,*)" "
			call print_interval_multiple(start,finish)

			!finally, close the file
			close(image_number)

			!add response counter to will
			!if this is a motivation network, write the individual oddsey number
			if (output_switcher=="motive") then
				call read_write(image_number,image_total,think,epoch,moves,"write",response_record=response_counter,illiad=oddsey)
			else if (output_switcher=="normal") then
				call read_write(image_number,image_total,think,epoch,moves,"write",response_record=response_counter)
			end if


		else

			!place all the information network in a text file
			!if this is a motivation network, write the individual oddsey number
			if (output_switcher=="motive") then
				call read_write(image_number,image_total,think,epoch,moves,"write",illiad=oddsey)
			else if (output_switcher=="normal") then
				call read_write(image_number,image_total,think,epoch,moves,"write")
			end if


		end if
		

	end if


end subroutine insanitorium_deluxe

end module in_search_of_sanity
