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

pure subroutine insanitorium_deluxe(think,oddsey,image_number,rows,columns,node_use_reward,&
	vision,response,vision_socket,response_socket,blood_rate,blood_rows,&
	blood_volume,blood_gradient,output_switcher,ran_count)
 	!$acc routine seq

	!network setup and reading
	integer,intent(in) :: rows, columns
	type(mind),intent(inout) :: think !think is the vision and response, motivate is the cash flow and neurochem controller
	real,intent(in) :: node_use_reward

	!sensing and response setup
	!think array interfaces
	integer,dimension(*),intent(in) :: vision(:) 
	integer,dimension(*),intent(inout) :: response(:)
	!motivate array interfaces
	integer,intent(inout) :: oddsey
	!ubiquitous
	integer :: vision_length, response_length
	integer,intent(in) :: vision_socket, response_socket

	!selecting and moving
	integer :: column_number, column_number_2

	!from the outside
	integer,parameter :: epoch_cutoff=100 !ubiqutous

	!blood
	integer,intent(in) :: blood_rate
	integer,intent(in) :: blood_rows
	real,intent(in) :: blood_volume, blood_gradient


	!output
	character(len=6),intent(in) :: output_switcher !the switch for normal and motivation networks

	!coarray specific
	integer,intent(in) :: image_number

	!randomiser
	integer,intent(inout) :: ran_count
	
	!accelerator subroutine and function declarations
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


	!define the size of the vision and response arrays
	vision_length=size(vision); response_length=size(response)




			
	!this makes the system yearn for happiness
	!must be run for each network
	if (output_switcher=="motive") then
		call raining_blood(vision,oddsey,image_number)
	end if
	call animus(think,oddsey)
		


	




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
		rows,columns,node_use_reward,&
		output_switcher,ran_count)





	!!!!!!!!!!!!!!!!!!!!!!!!!!
	!!! Motivation Decider !!!
	!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	
	!oddsey is the multiplier for the neurochem effect, as defined by the motivate network
	!oddsey is defined only in the motivate image

	if (output_switcher=="motive") then
		!find the bit position
		do column_number=1,size(response)
			if (response(column_number)==1) oddsey=column_number
		end do
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







	


end subroutine insanitorium_deluxe

end module in_search_of_sanity
