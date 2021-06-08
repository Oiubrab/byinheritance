module spiritechnology
use welcome_to_dying
!use darkness
implicit none
contains

!place input and output subroutines in this module

!this is it. This is the brain kernel. In this where the network is controled from. It is here that data is fed into and data comes out from.
!run this to evolve the network built outside. This will need a handshake and a neurochem value at some point

subroutine spiritech(thinking,blood_rate,response_socket,response_length,vision_length,vision_socket,blood_rows,epoch_cutoff,&
	blood_gradient,blood_volume,vision,response,rows,columns,&
	node_use_reward,image_number,motivate_nomotivate)

	!timing and controlling
	integer :: epoch,epoch_cutoff
	logical :: proaction
	character(len=6) :: motivate_nomotivate
	
	!test timing
	real :: start, finish, select_time
	
	!blood
	integer :: blood_rows
	real :: blood_volume,blood_gradient
	integer :: blood_rate
	
	!brain
	type(mind) :: thinking
	integer :: rows,columns
	real :: node_use_reward
	integer,allocatable :: transition(:,:)
	
	!sense and response
	integer :: response_socket,response_length,vision_length,vision_socket
	integer,dimension(*) :: vision(:),response(:)
	
	!test log
	logical :: testing,show_blood
	real :: delay_time
	
	!general position
	integer :: column_number,column_number_2,column_number_3,column_random_number
	integer :: row_number,row_number_2,row_random_number
	integer,dimension(columns) :: column_random
	integer,dimension(rows) :: row_random
	
	!parallelisation
	integer :: image_number
	
	!print*,"spirit",this_image(),rows,columns,blood_rows,response_socket
	select_time=0.0

	proaction=.false.
	epoch=0
	
	allocate(transition(columns,rows))

!	call print_network(image_number,epoch,vision,vision_socket,response,response_socket,&
!		thinking%brain_status,thinking%blood)

	!this is the new song
	do while (proaction .eqv. .false.)
	
		transition=0
	
		!print*,size(thinking%blood(:,1)),1,"spiritech"
	
		!increment epoch
		epoch=epoch+1

		!add the blood volume, i.e
		! 00000000
		! 00000000
		! 00000000
		! 01111110
!		if (epoch==1) then
!			!print*,size(thinking%blood(:,1)),3,response_length,response_socket,"spiritech"
!			do column_number=1,response_length
!				thinking%blood(plugin(column_number,response_socket,response_length,"brain"),blood_rows)=&
!					thinking%blood(plugin(column_number,response_socket,response_length,"brain"),blood_rows)+blood_volume	
!			end do	
!		
!			do column_number=1,vision_length
!				thinking%blood(plugin(column_number,vision_socket,vision_length,"brain"),1)=&
!					thinking%blood(plugin(column_number,vision_socket,vision_length,"brain"),1)*0.7
!			end do
!		end if

		!print*,"made it",this_image()
		!first, randomise random row list
		call randomised_list(row_random)
		 
		!now I shall send randomly selected neurons to have data moved
		row_loop: do row_number=1,rows



			!first, randomise column list for each row
			call randomised_list(column_random)
		
			column_loop: do column_number=1,columns
			

			
				!now, assign the random integer positional number to the requisite random number positional number holder number
				column_random_number=column_number!column_random(column_number)
				row_random_number=row_number!row_random(row_number)
	

				
				!move the blood around
				
!				if (epoch==1) call blood_mover(thinking%blood,column_random_number,row_random_number,blood_gradient)

call cpu_time(start)					
							
				!only act on neurons that have data in them
				if (thinking%brain_status(2,column_random_number,row_random_number)==1) then

					!here is the important subroutine call that moves the data depending on how fat the neuron is 
					!individual data may move several times each loop. Loop these loops for a truly random movement (feature, not bug) 
					call selector(thinking,column_random_number,row_random_number,node_use_reward,response,response_socket,transition)		

					!print*,select_time


					

					
					
				end if

call cpu_time(finish)
select_time=select_time+(finish-start)
				
				!the choosing weights reduce by one if they are not used and increase by node_use_reward-1 if they are used
				!this is done by subtracting one from all weights and adding node_use_reward to weights that are used
				call weight_reducer(thinking%brain_weight,column_random_number,row_random_number)


				
			end do column_loop



!			!move the blood on the extra blood row
!			if (row_number==rows) then
!				call randomised_list(column_random)
!				do column_number=1,columns
!					!now, assign the random integer positional number to the requisite random number positional number holder number
!					column_random_number=column_random(column_number)	
!					!move the blood around
!					call blood_mover(thinking%blood,column_random_number,blood_rows,blood_gradient)
!				end do
!			end if
		

			
		end do row_loop
		
		call conflict_check_resolve_move(thinking,transition,node_use_reward,response,response_socket)

!		call print_network(image_number,epoch,vision,vision_socket,response,response_socket,&
!			thinking%brain_status,thinking%blood)

		!response translation into movement
		!different rules for motivate and non motivate networks
		if (motivate_nomotivate=="normal") then
		
			!this condition picks up whether some response has been fed into the stop position (last position in the array)
			if (response(response_length)==1) then
				!stop the main loop
				proaction=.true.

			end if
		
		else if (motivate_nomotivate=="motive") then
		
			!response translation into movement
			!this do loop picks up whether some response has been fed into 
			search_loop: do column_number_2=1,response_length
				if (response(column_number_2)==1) then
					
					!stop the main loop
					proaction=.true.


				end if
			end do search_loop
			
		end if


		!if nothing has happened for epoch_cutoff, exit and output 0
		if (epoch>epoch_cutoff) then
			proaction=.true.
		end if
		

		
	end do

	!print*,size(thinking%blood(:,1)),2,"spiritech"

!	print*,"selector",image_number,select_time

end subroutine

end module
