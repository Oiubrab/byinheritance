module spiritechnology
use welcome_to_dying
use darkness
implicit none
contains

!place input and output subroutines in this module

!this is it. This is the brain kernel. In this where the network is controled from. It is here that data is fed into and data comes out from.
!run this to evolve the network built outside. This will need a handshake and a neurochem value at some point

subroutine spiritech(thinking,blood_rate,response_socket,response_length,vision_length,vision_socket,epoch_cutoff,&
	blood_gradient,blood_volume,vision,response,&
	node_use_reward,image_number,motivate_nomotivate,testicle)

	!timing and controlling
	integer :: epoch,epoch_cutoff
	logical :: proaction
	character(len=6) :: motivate_nomotivate
	
	!test timing
	real :: start, finish, select_time
	
	!blood
	real :: blood_volume,blood_gradient
	integer :: blood_rate
	
	!brain
	type(mind) :: thinking
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
	integer,allocatable :: column_random(:)
	integer,allocatable :: row_random(:)
	
	!parallelisation
	integer :: image_number
	
	!testing
	character(len=6) :: testicle
	
	!print*,"spirit",this_image(),rows,columns,blood_rows,response_socket
	select_time=0.0

	!allocate the randomisers
	allocate(row_random(size(thinking%brain_status(1,1,:))))
	allocate(column_random(size(thinking%brain_status(1,:,1))))	

	proaction=.false.
	epoch=0
	

	if (testicle=="test") then
		call cpu_time(start)
		call print_network(image_number,epoch,vision,vision_socket,response,response_socket,&
			thinking%brain_status,thinking%blood)
	end if


	!this is the new song
	do while (proaction .eqv. .false.)
	
		!print*,size(thinking%blood(:,1)),1,"spiritech"
	
		!increment epoch
		epoch=epoch+1
		
		call new_song(thinking,response,response_socket,response_length,node_use_reward)
		
		call weight_reducer(thinking%brain_weight)



		!response translation into movement
		!different rules for motivate and non motivate networks
		if (motivate_nomotivate=="normal") then
		
			!this condition picks up whether some response has been fed into the stop position (last position in the array)
			do column_number_2=1,size(response)
				if (response(column_number_2)==1) then
					!stop the main loop
					proaction=.true.

				end if
			end do
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
		
		if (testicle=="test") then
			if (proaction .eqv. .true.) then
				call cpu_time(finish)
				!print*,image_number,start,finish
				call print_network(image_number,epoch,vision,vision_socket,response,response_socket,&
					thinking%brain_status,thinking%blood,start,finish)
			else
				call print_network(image_number,epoch,vision,vision_socket,response,response_socket,&
					thinking%brain_status,thinking%blood)
			end if
		end if
		
	end do

	!print*,size(thinking%blood(:,1)),2,"spiritech"

!	print*,"selector",image_number,select_time

end subroutine

end module
