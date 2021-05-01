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
	vision,response,vision_socket,response_socket,blood_rate,&
	blood_volume,blood_gradient,neurodepth,output_switcher)

	implicit none

	!network setup and reading
	integer :: rows, columns, directions, neurodepth
	type(mind) :: think !think is the vision and response, motivate is the cash flow and neurochem controller
	logical :: file_exists
	real :: node_use_reward

	!sensing and response setup
	!think array interfaces
	integer,dimension(*) :: vision(:), response(:)
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

	!timing
	real :: start, finish, delay_time=0.0

	!output
	character(len=6) :: output_switcher !the switch for normal and motivation networks

	!coarray specific
	integer :: image_number,image_total
	character(len=20) :: will_file


	!blood rows is always rows +1
	blood_rows=rows+1
	!print*,"search1",rows,blood_rows,this_image(),response_socket

	!define the size of the vision and response arrays
	vision_length=size(vision); response_length=size(response)


	!allocate the network to the dimensions fed into the subroutione
	allocate(think%brain_status(2,columns,rows)) !allocate the brain data and direction status, 1 is for the origin direction of data, 2 is for data status
	allocate(think%brain_weight(directions,directions,columns,rows)) !allocate the brain direction weighting. 8,:,:,: holds the weights from origin :,x,:,: to point x,:,:,:
	allocate(think%blood(columns,blood_rows)) !allocate the gradient variable, extra row for response array
	allocate(think%neurochem(2,neurodepth,columns,rows)) !allocate the reward variable, 1,:,:,: is for origin, 2,:,:,: is for point. :,10,:,: is the weight ladder


	!print*,size(think%blood(1,:))




	!this is it
	
	

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

		call read_write(image_number,image_total,think,"read")

			
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
		call initialiser(think,response,blood_volume,response_socket)

		

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
	call spiritech(think,blood_rate,response_socket,response_length,vision_length,&
		vision_socket,blood_rows,epoch_cutoff,blood_gradient,blood_volume,vision,response,&
		rows,columns,node_use_reward,image_number,output_switcher)





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

	


	!place all the information network in a text file
	call read_write(image_number,image_total,think,"write")




end subroutine insanitorium_deluxe

end module in_search_of_sanity
