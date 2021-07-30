module spiritechnology
use welcome_to_dying
use darkness
implicit none
contains

!place input and output subroutines in this module

!this is it. This is the brain kernel. In this where the network is controled from. It is here that data is fed into and data comes out from.
!run this to evolve the network built outside. This will need a handshake and a neurochem value at some point

subroutine spiritech(thinking,blood_rate,response_socket,response_length,vision_length,vision_socket,blood_rows,epoch_cutoff,&
	blood_gradient,blood_volume,vision,response,rows,columns,&
	node_use_reward,image_number,motivate_nomotivate,testicle)

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
	
	!testing
	character(len=6) :: testicle
	
	!print*,"spirit",this_image(),rows,columns,blood_rows,response_socket
	select_time=0.0

	proaction=.false.
	epoch=0
	
	allocate(transition(columns,rows))

	if (testicle=="test") then
		call print_network(image_number,epoch,vision,vision_socket,response,response_socket,&
			thinking%brain_status,thinking%blood)
	end if


	!this is the new song
	do while (proaction .eqv. .false.)
	
		transition=0
	
		!print*,size(thinking%blood(:,1)),1,"spiritech"
	
		!increment epoch
		epoch=epoch+1
		
		call new_song(thinking,response,response_socket,response_length,rows,columns,node_use_reward)
		
		call weight_reducer(thinking%brain_weight)

		if (testicle=="test") then
			call print_network(image_number,epoch,vision,vision_socket,response,response_socket,&
				thinking%brain_status,thinking%blood)
		end if

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
