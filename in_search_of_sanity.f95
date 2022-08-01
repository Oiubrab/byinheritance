module in_search_of_sanity
use welcome_to_dying
use spiritechnology
use the_sound_of_perserverance
contains

!this subroutine is the main kernalised routine for brain creation and action
!here, each network is created as a will file (text) and is propogated through
!from one input to one output

!run this script in parallel, with inputs connected to the outputs of other networks for a truly integrated experience

!Note, there are two versons here:
!Motive - input feeds into a single data output
!Normal - input feeds in and output keeps building until the stop output is activated
!motivate_network is the feed in switch that determines whether the network is normal or motive 

subroutine insanitorium_deluxe(initial,think,oddsey,image_number,node_use_reward,&
	vision,response,vision_socket,response_socket,blood_rate,&
	blood_volume,blood_gradient,epoch_cutoff,motivate_network,test)

	implicit none

	!network setup and reading
	type(mind) :: think !think is the vision and response, motivate is the cash flow and neurochem controller
	logical :: initial
	real :: node_use_reward

	!sensing and response setup
	!think array interfaces
	integer,dimension(*) :: vision(:), response(:)
	!motivate array interfaces
	logical :: motivate_network
	integer :: oddsey
	!ubiquitous
	integer :: vision_length, response_length
	integer :: vision_socket, response_socket

	!selecting and moving
	integer :: column_number, column_number_2, row_number
	integer :: moves
	integer :: column_random_number, row_random_number
	integer,allocatable :: column_random(:)
	integer,allocatable :: row_random(:)
	integer,allocatable :: blood_row_random(:)

	!from the outside
	integer :: epoch_cutoff


	!blood
	integer :: blood_rate
	real :: blood_volume, blood_gradient

	!timing
	real :: start, finish
	
	!coarray specific
	integer :: image_number
	
	!testing
	logical ::  test
	integer :: testicles

	allocate(column_random(size(think%brain_status(1,:,1))))
	allocate(row_random(size(think%brain_status(1,1,:))))
	allocate(blood_row_random(size(think%blood(1,:))))

	!print*,"search1",rows,size(think%blood(1,:)),this_image(),response_socket

	!define the size of the vision and response arrays
	vision_length=size(vision); response_length=size(response)

	!this is it


	!fuck you
	call random_seed()
	

	!took sight and feel input from here into "at the heart of winter"
	
	!print*,initial,image_number

	if (initial .eqv. .false.) then
		!zero out the response first
		!for think (1)
		response=0
		!open the test log


		call cpu_time(start)


			
		!this makes the system yearn for happiness
		!must be run for each network
		if (motivate_network .eqv. .true.) then
			call flesh_and_the_power_it_holds(think,vision,oddsey,image_number)
		end if
		call animus(think,oddsey)

		
	!Otherwise, if this is the first time this network is activated, it has to be initialised
	else
		

		
		call initialiser(think,response,blood_volume,response_socket)
		call cpu_time(start)
		!add the necessary blood to the response and vision input/output 
		do column_number=1,response_length
			think%blood(plugin(column_number,response_socket,response_length,"brain"),size(think%blood(1,:)))=&
				think%blood(plugin(column_number,response_socket,response_length,"brain"),size(think%blood(1,:)))+blood_volume	
		end do	
	
		do column_number=1,vision_length
			think%blood(plugin(column_number,vision_socket,vision_length,"brain"),1)=&
				think%blood(plugin(column_number,vision_socket,vision_length,"brain"),1)*0.7
		end do	
		

		!initialise the blood
		call blood_mover(think%blood,blood_gradient)



	end if

	


!	print*,vision

	!injection, from vision into brain
	!inject data from the vission arrays into the networks

	do column_number=1,vision_length
		if (vision(column_number)==1) then	
			think%brain_status(1,plugin(column_number,vision_socket,vision_length,"brain"),1)=2
			think%brain_status(2,plugin(column_number,vision_socket,vision_length,"brain"),1)=1
		end if
	end do

	if (image_number==1) then
!		print*,image_number,vision_socket,vision
		do testicles=1,size(think%brain_status(2,1,:))
				print'(15I3)',think%brain_status(2,:,testicles)
		end do
!		print*,response
	end if
	
	
	
	
		

	!!!!!!!!!!!
	!!! Die !!!
	!!!!!!!!!!!
	!This is the kernal, the grand daddy of this whole rotten affair
	!this subroutine moves through the network and blood and propogates all data movement
	call spiritech(think,blood_rate,response_socket,response_length,vision_length,&
		vision_socket,epoch_cutoff,blood_gradient,blood_volume,vision,response,&
		node_use_reward,image_number,motivate_network,test,oddsey)

	
!	print*,"spiritech",image_number,finish-start



	!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!! Motivation Decider !!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	
	!oddsey is the multiplier for the neurochem effect, as defined by the motivate network
	!oddsey is defined only in the motivate image
	if (motivate_network .eqv. .true.) then
		oddsey=findloc(response,1,dim=1)
		!if no data comes through, don't change the weights
		if (oddsey==0) then
			oddsey=(size(response)/2)+1
		end if
		!right is higher motivation, left is lower motivation
		!midpoint is zero motivation and anything to the left is negative
		oddsey=oddsey-(size(response)/2)+1
	end if


	

	!call cpu_time(start)
	!place all the information network in a text file
	call cpu_time(finish)
	!call print_interval(start,finish)
	!print*,image_number




end subroutine insanitorium_deluxe

end module in_search_of_sanity
