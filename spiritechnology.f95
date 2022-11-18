module spiritechnology
use welcome_to_dying
use the_sound_of_perserverance
implicit none
contains

!place input and output subroutines in this module

!this is it. This is the brain kernel. In this where the network is controled from. It is here that data is fed into and data comes out from.
!run this to evolve the network built outside. This will need a handshake and a neurochem value at some point

subroutine spiritech(thinking,blood_rate,response_socket,response_length,vision_length,vision_socket,epoch_cutoff,&
	blood_gradient,blood_volume,vision,response,&
	node_use_reward,image_number,motivate_network,testicle,oddsey)

	!timing and controlling
	integer :: epoch,epoch_cutoff,oddsey
	logical :: proaction,motivate_network
	
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
	logical :: testicle
	integer :: testicles
	character(len=100) :: network_tit
	character(len=100) :: network_columns
	
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
		
	if ((image_number==1) .and. (vision_length==15)) then
		print*,image_number,vision_socket,vision
		do testicles=1,size(thinking%brain_status(2,1,:))
			print'(17I3)',thinking%brain_status(2,:,testicles)
		end do
		print*,response
	end if
		
		!run the netwirk
		call new_song(thinking,response,response_socket,response_length,node_use_reward)
!		call blood_mover(thinking%blood,blood_gradient)
		!if this is a motivator, run the base motivator
		if (motivate_network .eqv. .true.) then
			call flesh_and_the_power_it_holds(thinking,vision,oddsey,image_number)
		end if
		!run the motivator
		call animus(thinking,oddsey)
		
	if ((image_number==1) .and. (vision_length==15)) then
		print*,image_number,vision_socket,vision
		do testicles=1,size(thinking%brain_status(2,1,:))
				print'(17I3)',thinking%brain_status(2,:,testicles)
		end do
		print*,response
	end if
		
		write(network_columns,*) size(thinking%brain_status(2,:,1))
		!data printing
		if (testicle .eqv. .true.) then
!			call sleep(1)
!			open(unit=image_number, file=network_tit,access='append')
			do row_number=1,size(thinking%brain_status(1,1,:))
				print'('//trim(network_columns)//'I2)', thinking%brain_status(2,:,row_number)
!				write(image_number,*) thinking%brain_status(2,:,row_number)
			end do
			print*," "
			do row_number=1,size(thinking%brain_status(1,1,:))
				print'('//trim(network_columns)//'F5.2)', thinking%blood(:,row_number)
			end do
			print*," "
!			write(image_number,*) " "
!			close(image_number)
		end if
		
		call weight_reducer(thinking%brain_weight)

		!count the active neurons and apply a function to regulate the number
		call regulators(thinking)

		!response translation into movement
		!different rules for motivate and non motivate networks
		if (motivate_network .eqv. .false.) then
		
			!this condition picks up whether some response has been fed into the stop position (last position in the array)
			do column_number_2=1,size(response)
				if (response(column_number_2)==1) then
					!stop the main loop
					proaction=.true.

				end if
			end do
		else if (motivate_network .eqv. .true.) then
		
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
