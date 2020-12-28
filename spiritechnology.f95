module spiritechnology
use welcome_to_dying
implicit none
contains

!place input and output subroutines in this module

!this is it. This is the brain kernel. In this where the network is controled from. It is here that data is fed into and data comes out from.
!run this to evolve the network built outside. This will need a handshake and a neurochem value at some point

subroutine spiritech(epoch,think,blood_rate,response_socket,response_length,vision_length,vision_socket,blood_rows,epoch_cutoff,&
	blood_gradient,blood_volume,vision,response,response_counter,rows,columns,moves,testing,show_blood,delay_time,epoch_start)

	!timing and controlling
	integer :: moves, epoch, epoch_start,epoch_cutoff
	logical :: proaction
	
	!blood
	integer :: blood_rows
	real :: blood_volume,blood_gradient
	integer :: blood_rate
	
	!brain
	type(mind) :: think
	integer :: rows,columns
	real :: node_use_reward
	
	!sense and response
	integer :: response_socket,response_length,vision_length,vision_socket
	integer,allocatable :: vision(:),response(:),response_counter(:,:)
	
	!test log
	logical :: testing,show_blood
	real :: delay_time
	
	!general position
	integer :: column_number,column_number_2,column_number_3,column_random_number
	integer :: row_number,row_number_2,row_random_number
	integer,dimension(columns) :: column_random
	integer,dimension(rows) :: row_random
	

	proaction=.false.

	!this is the new song
	do while (proaction .eqv. .false.)

		!increment epoch
		epoch=epoch+1

		!add the blood volume, i.e
		! 00000000
		! 00000000
		! 00000000
		! 01111110
		if (mod(epoch,blood_rate)==1) then
			do column_number=1,response_length
				think%blood(plugin(column_number,response_socket,response_length,"brain"),blood_rows)=&
					think%blood(plugin(column_number,response_socket,response_length,"brain"),blood_rows)+blood_volume	
			end do	
		end if
		do column_number=1,vision_length
			think%blood(plugin(column_number,vision_socket,vision_length,"brain"),1)=&
				think%blood(plugin(column_number,vision_socket,vision_length,"brain"),1)*0.7
		end do

		!first, randomise random row list
		call randomised_list(row_random)
		 
		!now I shall send randomly selected neurons to have data moved
		row_loop: do row_number=1,rows

			!first, randomise column list for each row
			call randomised_list(column_random)
		
			column_loop: do column_number=1,columns
			
				!now, assign the random integer positional number to the requisite random number positional number holder number
				column_random_number=column_random(column_number)
				row_random_number=row_random(row_number)
				
				!move the blood around
				call blood_mover(think%blood,column_random_number,row_random_number,blood_gradient)
				
							
				!only act on neurons that have data in them
				if (think%brain_status(2,column_random_number,row_random_number)==1) then

					!here is the important subroutine call that moves the data depending on how fat the neuron is 
					!individual data may move several times each loop. Loop these loops for a truly random movement (feature, not bug) 
					call selector(think,column_random_number,row_random_number,node_use_reward,response,response_socket,testing)		
								
					!lag if necessary
					if (testing .eqv. .true.) then
						call delay(delay_time)
					end if
					
					!increase the moves count
					moves=moves+1
				
					!print each step
					if (testing .eqv. .true.) then
						write(1,*)"By Inheritance"
						write(1,'(A15,I0,A8,I0)')"Brain moves: ",moves,"Epoch: ",epoch
						if (show_blood .eqv. .true.) then
							call print_network(vision,vision_socket,response,response_socket,think%brain_status,think%blood)
						else
							call print_network(vision,vision_socket,response,response_socket,think%brain_status)
						end if
						!keep track of response
						do column_number_2=1,response_length
							if (response(column_number_2)==1) then
								do column_number_3=1,vision_length
									if (vision(column_number_3)==1) then
										response_counter(column_number_2,column_number_3)=&
											response_counter(column_number_2,column_number_3)+1
									end if
								end do
							end if
						end do							
					end if	
						
					!print*,think%neurochem
					
					!response translation into movement
					!this do loop picks up whether some response ha been fed into 
					search_loop: do column_number_2=1,response_length
						if (response(column_number_2)==1) then
							
							!stop the main loop
							proaction=.true.
							exit row_loop

						end if
					end do search_loop
					
				end if
				
				!the choosing weights reduce by one if they are not used and increase by node_use_reward-1 if they are used
				!this is done by subtracting one from all weights and adding node_use_reward to weights that are used
				call weight_reducer(think%brain_weight,column_random_number,row_random_number)
				
			end do column_loop

			!move the blood on the extra blood row
			if (row_number==rows) then
				call randomised_list(column_random)
				do column_number=1,columns
					!now, assign the random integer positional number to the requisite random number positional number holder number
					column_random_number=column_random(column_number)	
					!move the blood around
					call blood_mover(think%blood,column_random_number,blood_rows,blood_gradient)
				end do
			end if
			
		end do row_loop


		!if nothing has happened for epoch_cutoff, exit and output 0
		if ((epoch-epoch_start)>epoch_cutoff) then
			proaction=.true.
		end if
		
	end do


end subroutine

end module
