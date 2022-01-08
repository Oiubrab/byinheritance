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
	
	!general position
	integer :: column_number,column_number_2,column_number_3,column_random_number
	integer :: row_number,row_number_2,row_random_number
	integer,allocatable :: column_random(:)
	integer,allocatable :: row_random(:)
	
	!parallelisation
	integer :: image_number
	
	!testing
	character(len=6) :: testicle
	character(len=100) :: network_tit
	
	!print*,"spirit",this_image(),rows,columns,blood_rows,response_socket
	select_time=0.0

	!setup the network title
	write(network_tit,"(A5,I0,A4)")"think",image_number,".txt"

	!allocate the randomisers
	allocate(row_random(size(thinking%brain_status(1,1,:))))
	allocate(column_random(size(thinking%brain_status(1,:,1))))	

	proaction=.false.
	epoch=0


	!this is the new song
	do while (proaction .eqv. .false.)
	
		!increment epoch
		epoch=epoch+1
		
		call new_song(thinking,response,response_socket,response_length,node_use_reward)
		
		!data printing
		if (testicle=="test") then
			open(unit=image_number, file=network_tit)
			do row_number=1,size(thinking%brain_status(1,1,:))
				write(image_number,*) thinking%brain_status(2,:,row_number)
			end do
			close(image_number)
		end if
		
		call weight_reducer(thinking%brain_weight)

		!count the active neurons and apply a function to regulate the number
		call regulators(thinking)

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
		
		
	end do

	!print*,size(thinking%blood(:,1)),2,"spiritech"

!	print*,"selector",image_number,select_time

end subroutine

end module
