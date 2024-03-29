module welcome_to_dying

!define type for brain
type mind
	integer,allocatable :: brain_status(:,:,:) !allocate the brain data and direction status, 1 is for the origin direction of data, 2 is for data status
	real,allocatable :: brain_weight(:,:,:,:) !allocate the brain direction weighting. 8,:,:,: holds the weights from origin :,x,:,: to point x,:,:,:
	real,allocatable :: blood(:,:) !allocate the gradient variable, extra row for response array
	integer,allocatable :: neurochem(:,:,:,:) !allocate the reward variable, 1,:,:,: is for origin, 2,:,:,: is for point. :,10,:,: is the weight ladder
end type mind

!define input output type
type see_saw
	integer,allocatable :: vision(:),response(:)
end type see_saw

contains

!this is where the magic happens



















!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!housekeeping - time and metadata!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!










!returns a list of sequential numbers up to a length defined by the array input, in a list where the order of the numbers has been randomised
!master_killer is the allocatable array argument
subroutine randomised_list(master_killer)

	integer,dimension(*),intent(inout) :: master_killer(:)
	integer :: roger_explosion, length, pos_hold
	real :: despair

		length=size(master_killer)

		!make sequential array of length defined by the array size
		do roger_explosion=1,length
			master_killer(roger_explosion)=roger_explosion
		end do
	
		!swap elements until the list is randomized
		do roger_explosion=1,length
			call RANDOM_NUMBER(despair)
			pos_hold=master_killer(roger_explosion)
			master_killer(roger_explosion)=master_killer(int(length*despair)+1)
			master_killer(int(length*despair)+1)=pos_hold
		end do

end subroutine randomised_list



!read in the network from a text file or write out to a text file
subroutine read_write(imagine,imagination,think,direction)
	type(mind) :: think
	integer :: epoch,imagine,imagination
	character(len=20) :: willfull
	character(len=*) :: direction
	integer :: column,row
	
	write(willfull,"(A4,I0,A4)") "will",imagine,".txt"
	!print*,imagine,willfull,"read_write"
	
	if (direction=="read") then
	
		!retrieve previous network
		open(unit=imagination+imagine,file=willfull)
		read(imagination+imagine,*) think%brain_status
		read(imagination+imagine,*) think%brain_weight
		read(imagination+imagine,*) think%blood
		read(imagination+imagine,*) think%neurochem
		close(imagination+imagine)
		
	else if (direction=="write") then
	
		!write the networks to file
		open(unit=2*imagination+imagine,file=willfull)
		write(2*imagination+imagine,*) think%brain_status
		write(2*imagination+imagine,*) think%brain_weight
		write(2*imagination+imagine,*) think%blood
		write(2*imagination+imagine,*) think%neurochem
		close(2*imagination+imagine)
	
	end if
	
end subroutine read_write





















!!!!!!!!!
!!maths!!
!!!!!!!!!


!this function either applies a sigmoid (forward) or inverse sigmoid (reverse) function depending on flow variable
!note, inverse sigmoid only goes up to 16*range_stretch
function sigmoid(insig,flow,range_stretch,domain_stretch,range_shift,domain_shift) result(outsig)
	
	real,intent(in) :: insig
	real,intent(in) :: range_stretch,domain_stretch,range_shift,domain_shift
	real :: outsig
	character(len=*) :: flow

	!sigmoid
	if (flow=="forward") then
		outsig=range_shift+(range_stretch/(1.+exp(domain_shift-(1./domain_stretch)*insig)))
	!inverse sigmoid
	else if (flow=="reverse") then
		outsig=(log((insig-range_shift)/(range_stretch+range_shift-insig))+domain_shift)/(1./domain_stretch)
		if ((1./outsig)==0.) then
			outsig=16.
		end if
	end if

end function sigmoid





!!!!!!!!!!!!!!!!
!!!conversion!!!
!!!!!!!!!!!!!!!!


!this function takes a positional binary array that represents a percentage (-100,100) and returns an approximation of that percentage
function position_to_percentage(positional) result(percent)

	integer,dimension(*),intent(in) :: positional(:)
	integer :: percent,pos
	real :: data_width
	
	!find the width between each binary position
	!note, total width is (-100,100)
	data_width=200.0/float(size(positional))
	
	!find the data position and convert into percentage approximation
	do pos=1,size(positional)
		if (positional(pos)==1) then
			percent=-100+int((float(pos-1)*data_width)+(((float(pos)*data_width)-(float(pos-1)*data_width))/2.0))
		end if
	end do
			
end function




!this function takes a binary array and outputs an integer
!the last bit represents pos (1) or neg (0)
!note; the binary array must be inputted in format left:smallest to right:highest
function binary_to_decimal(binary_array) result(decimal)

	integer,dimension(*),intent(in) :: binary_array(:)
	integer :: order, decimal, binary_size
	
	!size the array and zero the integer
	binary_size=size(binary_array)
	decimal=0
	
	!conversion algorithm: if binary position is 1, add 2^order to the integer untill all the positions (except last) have been taken
	do order=0, binary_size-2
		decimal = decimal + binary_array(order+1)*(2**order)
		!print*,decimal,binary_array(order+1),((binary_array(order+1))*2)
	end do
	
	!make negative integer if the negative trigger is set
	if (binary_array(binary_size)==0) then
		decimal=decimal*(-1)
	end if
	
end function
		





















!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!breaking the fourth wall - array address finding!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!Data Direction Map
!the selection below is represented by the point pointing in the fashion

! 1 2 3
! 4 x 5
! 6 7 8

!where x is the column,row position of the currently activated node





!takes in the column, row position of the input neuron and a pointing number. Outputs the column or row of the output neuron, depending on rowcolumn variable input
function point_to_neuron(column_in,row_in,point,rowcolumn) result(finder)

	integer,intent(in) :: column_in, row_in, point
	character(len=*) :: rowcolumn
	integer :: finder

	!see data direction map for understanding the numerical pattern below
	
	if (rowcolumn=="row") then
	
		if (point<=3) then
			finder=row_in-1
		else if (point>=6) then
			finder=row_in+1
		else
			finder=row_in
		end if
	
	else if (rowcolumn=="column") then
	
		if ((point==1) .or. (point==4) .or. (point==6)) then
			finder=column_in-1
		else if ((point==3) .or. (point==5) .or. (point==8)) then
			finder=column_in+1
		else
			finder=column_in
		end if
		
	end if
	
end function point_to_neuron





!this function turns the destination label number of the data from it's old home into the origin of the data in it's new home, as per data direction map above
!assumes 8 directions
function point_origin(point) result(origin)

	integer :: point, origin
	
	if (point==1) then
		origin=8
	else if (point==2) then
		origin=7
	else if (point==3) then
		origin=6
	else if (point==4) then
		origin=5
	else if (point==5) then
		origin=4
	else if (point==6) then
		origin=3
	else if (point==7) then
		origin=2
	else if (point==8) then
		origin=1
	end if
	
end function point_origin




!this function takes a weight address (uses 8 directions as assigned in the diagram above) and gives the direction corresponding to that weight
function weight_direction(weighting) result(resultant)

	integer :: weighting, resultant, modding=8

	resultant=mod(weighting,modding)
	if (resultant==0) then
		resultant=modding
	end if

end function weight_direction



!this function calculates the position of the corresponding brain/array column, given an array/brain column and a socket
!socket number represents where the middle of the corresponding array meets the brain network
!label is what is meant to be found, i.e brain label will give the brain column for a given response array position
function plugin(in_pos,socket,com_length,label) result(out_pos)

	character(len=*) :: label
	integer,intent(in) :: in_pos,socket,com_length
	integer :: out_pos
	
	if (label=="brain") then
		out_pos=socket-((com_length/2)+1)+in_pos
	else if (label=="array") then
		out_pos=in_pos-socket+((com_length/2)+1)
		!print*,"here",in_pos,socket,((com_length/2)+1),out_pos
	end if
	
end function plugin





















!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!planting and payoff - initialisation and data moving!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





!this subroutine regulates the amount of data passing through the network
subroutine regulators(thunk)

	type(mind) :: thunk
	
	if (sum(thunk%brain_status(2,:,:))>20) then
		!do something
	end if
	
end subroutine




!this subroutine will take a position and roll on a direction for the data
!if mask is present, it's corresponding direction will be ignored
subroutine tormenting_the_innocent(thinking,transition,column,row,mask)

	type(mind) :: thinking
	integer,optional :: mask
	integer,intent(in) :: column,row
	integer :: rung
	integer,dimension(*) :: transition(:,:)
	real :: fuck
	real, dimension(8) :: ladder
	integer,dimension(8) :: connection_translation



	!setup the selection variables
	call random_number(fuck) !the selector variable
	call randomised_list(connection_translation) !the position of the neuron direction is randomised in the ladder with this key
	ladder=0


		
	!setup the selection ladder
	!as each rung is setup on top of the other, the first rung needs special setup
	if (thinking%brain_weight(connection_translation(1),thinking%brain_status(1,column,row),column,row)>0.5) ladder(1)=&
		thinking%brain_weight(connection_translation(1),thinking%brain_status(1,column,row),column,row)*&
		thinking%blood(point_to_neuron(column,row,connection_translation(1),"column"),&
		point_to_neuron(column,row,connection_translation(1),"row"))
	!now, setup the rest of the rungs on top of it	
	do rung=2, 8, 1

		if (present(mask)) then
			if ((thinking%brain_weight(connection_translation(rung),thinking%brain_status(1,column,row),&
				column,row)<0.5) .or. (mask==connection_translation(rung))) then
				
				ladder(rung)=ladder(rung-1)
				
			else
			
				ladder(rung)=ladder(rung-1)+thinking%brain_weight(connection_translation(rung),thinking%brain_status(1,column,row),&
					column,row)*thinking%blood(point_to_neuron(column,row,connection_translation(rung),"column"),&
					point_to_neuron(column,row,connection_translation(rung),"row"))
			end if
			
		else
			if ((thinking%brain_weight(connection_translation(rung),thinking%brain_status(1,column,row),&
				column,row)<0.5)) then
				
				ladder(rung)=ladder(rung-1)
				
			else
			
				ladder(rung)=ladder(rung-1)+thinking%brain_weight(connection_translation(rung),thinking%brain_status(1,column,row),&
					column,row)*thinking%blood(point_to_neuron(column,row,connection_translation(rung),"column"),&
					point_to_neuron(column,row,connection_translation(rung),"row"))
			end if		
		end if
			
	end do
	
	!expand the fuck to reach the top of the ladder and use that number to select one of the rungs, placing it's corresponding pointer in the transition matrix
	do rung=1, 8, 1
		if( fuck*ladder(8)<=ladder(rung) ) then
			transition(column,row)=connection_translation(rung)
			exit
		end if
	end do

end subroutine
	





!this subroutine takes in the network and propogates the next battery of movements
subroutine new_song(think,response,response_socket,response_length,reward)

	type(mind) :: think
	integer,dimension(*) :: response
	integer :: response_socket,response_length
	real :: reward,fuck_me
	integer,allocatable :: transition(:,:)
	integer,dimension(2,8) :: conflict_list !(1,x) is column, (2,x) is row
	real, dimension(8) :: ladder
	integer :: rung,column_count,row_count,column_check,row_check,error_count,rank_size,rank
	logical :: error_check,catch
	
	allocate(transition(size(think%brain_status(1,:,1)),size(think%brain_status(1,1,:))))
	
	!selector
	transition=0
	do row_count=1,size(think%brain_status(1,1,:))
		do column_count=1, size(think%brain_status(1,:,1))
			
			if( think%brain_status(2,column_count,row_count)==1 ) then
				
				call tormenting_the_innocent(think,transition,column_count,row_count)
				
			end if
			
		end do
	end do



	!checker
	error_check=.true.
	do while (error_check .eqv. .true.)
		error_check=.false.
		do row_count=1, size(think%brain_status(1,1,:)), 1
			do column_count=1, size(think%brain_status(1,:,1)), 1
				if( think%brain_status(2,column_count,row_count)==1 ) then
				
					!reset error checking variables
					conflict_list=0
					error_count=1
					
					do row_check=row_count-2, row_count+2, 1
						do column_check=column_count-2, column_count+2, 1
						
							!check if the chcek is in the network
							if( (row_check>=1) .and. (column_check>=1) .and. (row_check<=size(think%brain_status(1,1,:))) .and. &
								(column_check<=size(think%brain_status(1,:,1))) .and. ((row_check/=row_count) .or. (column_check/=column_count)) ) then
								
								!check if there is a datum that is going to the same place
								if ( (point_to_neuron(column_check,row_check,transition(column_check,row_check),"row")==&
									point_to_neuron(column_count,row_count,transition(column_count,row_count),"row")) .and. &
									(point_to_neuron(column_check,row_check,transition(column_check,row_check),"column")==&
									point_to_neuron(column_count,row_count,transition(column_count,row_count),"column")) .and. &
									(think%brain_status(2,column_check,row_check)==1) ) then
									
									!print*,"definitely found",column_check,row_check,column_count,row_count
									!print*," "
									
									!if this is the first error, add the position of the count and check. Otherwise, just add the check
									if (error_count==1) then
										conflict_list(:,1)=[column_count,row_count]
										conflict_list(:,2)=[column_check,row_check]
										error_count=3

									else
										conflict_list(:,error_count)=[column_check,row_check]
										error_count=error_count+1	

									end if
									
									!signal an error
									error_check=.true.
									
								end if
							end if
							
							
							
						end do
					end do
					
					!roll on the the weights
					call random_number(fuck_me)
					
					ladder=0
					!setup the selection ladder
					!as each rung is setup on top of the other, the first rung needs special setup
					if ((conflict_list(1,1)/=0) .and. (conflict_list(1,2)/=0)) ladder(1)=think%brain_weight(transition(conflict_list(1,1),&
						conflict_list(2,1)),think%brain_status(1,conflict_list(1,1),conflict_list(2,1)),conflict_list(1,1),conflict_list(2,1))
					!now, setup the rest of the rungs on top of it	
					do rung=2, 8, 1
						
						if ((conflict_list(1,rung)==0) .and. (conflict_list(2,rung)==0)) then
							ladder(rung)=ladder(rung-1)						
						else
							!print*,rung,conflict_list
							ladder(rung)=ladder(rung-1)+think%brain_weight(transition(conflict_list(1,rung),conflict_list(2,rung)),&
								think%brain_status(1,conflict_list(1,rung),conflict_list(2,rung)),conflict_list(1,rung),conflict_list(2,rung))
						end if
							
					end do		
					
					
					!expand the fuck to reach the top of the ladder and use that number to select one of the rungs
					!take this rung to be the neuron that gets to send it's datum to the conflicting destination
					!all other neurons must re-roll
					catch=.false.
					do rung=1, 8, 1
						if( (fuck_me*ladder(8)<=ladder(rung)) .and. (catch .eqv. .false.)  ) then
							catch=.true.
						else if ((conflict_list(1,rung)/=0) .and. (conflict_list(2,rung)/=0)) then
						
							call tormenting_the_innocent(think,transition,conflict_list(1,rung),conflict_list(2,rung),&
								transition(conflict_list(1,rung),conflict_list(2,rung)))
					
						end if
					end do		
					
				end if
			end do
		end do
	end do


	!mover
	do row_count=1,size(transition(1,:))
		do column_count=1,size(transition(:,1))	

						
			!neurochem changes
			if (transition(column_count,row_count)>0) then
			
				!add data to neurochem
				!first, move each rank down one rung and push the last rank off the ladder
				rank_size=size(think%neurochem(1,:,1,1))
				do rank=1,rank_size-1
					think%neurochem(1,rank_size-rank+1,column_count,row_count)=think%neurochem(1,rank_size-rank,column_count,row_count)
					think%neurochem(2,rank_size-rank+1,column_count,row_count)=think%neurochem(2,rank_size-rank,column_count,row_count)
				end do
				!now, move the current activation to the top rung
				think%neurochem(1,1,column_count,row_count)=think%brain_status(1,column_count,row_count)
				think%neurochem(2,1,column_count,row_count)=transition(column_count,row_count)	
			
			end if
			
			
			
			
			!brain status changes
			!if the datum is leaving, reward the neuronal path and zero the neuron
			if (transition(column_count,row_count)>0) then	 
				think%brain_weight(transition(column_count,row_count),think%brain_status(1,column_count,row_count),column_count,row_count)=&
					think%brain_weight(transition(column_count,row_count),think%brain_status(1,column_count,row_count),column_count,row_count)&
					+reward
				think%brain_status(2,column_count,row_count)=0
				think%brain_status(1,column_count,row_count)=0
			end if
			!now, find if another datum is moving in and change the address accordingly
			do row_check=row_count-1,row_count+1
				do column_check=column_count-1,column_count+1
					if ((row_check>=1) .and. (column_check>=1) .and. (row_check<=size(transition(1,:))) .and. &
						(column_check<=size(transition(:,1))) .and. ((row_check/=row_count) .or. (column_check/=column_count))) then
						
						if ((transition(column_check,row_check)>0) .and. (point_to_neuron(column_check,row_check,transition(column_check,row_check),&
							"row")==row_count) .and. (point_to_neuron(column_check,row_check,transition(column_check,row_check),"column")==&
							column_count)) then
									
							!add data and position indicator to targeted neuron	
							think%brain_status(2,column_count,row_count)=1
							think%brain_status(1,column_count,row_count)=point_origin(transition(column_check,row_check))
							
						end if
					end if
				end do
			end do
							
						
		!response changes
		do column_check=1,size(transition(:,1))
			if (point_to_neuron(column_check,size(think%brain_status(1,1,:)),transition(column_check,&
				size(think%brain_status(1,1,:))),"row")==size(think%brain_status(1,1,:))+1) then
				
				response(plugin(point_to_neuron(column_check,size(think%brain_status(1,1,:)),&
					transition(column_check,size(think%brain_status(1,1,:))),"column"),&
					response_socket,response_length,"array"))=1
					
			end if
		end do
			

		
		end do
	end do	
	
end subroutine
	



!this subroutine takes in a neuron from the blood network and moves it around
subroutine blood_mover(blood,gradient)

	real,dimension(*) :: blood(:,:)
	integer,allocatable :: row_randomised(:), column_randomised(:),row_step(:),column_step(:)
	integer :: column_num,row_num,row_select,column_select
	real :: hope, fear, dist, distil, transition, transition_normal, transition_combine, transition_square
	real :: gradient !controls how quickly blood flows. bigger is faster
	integer :: rows,columns,row_number_2,column_number_2,row_number_1,column_number_1
	
	rows=size(blood(1,:)); columns=size(blood(:,1))

	!make the random arrays as big as columns and rows
	allocate(row_randomised(rows))
	allocate(column_randomised(columns))
	allocate(row_step(rows))
	allocate(column_step(columns))
	
	!pick a random row
	call randomised_list(row_step)
	do row_number_1=1,rows
	row_num=row_step(row_number_1)
	
		!pick a random column
		call randomised_list(column_step)
		do column_number_1=1,columns
		column_num=column_step(column_number_1)
			!randomise the row array
			call randomised_list(row_randomised)
			
			!engage every blood neuron around the neuron in question
			do row_select=1,rows
				!first, randomise the row selection
				row_number_2=row_randomised(row_select)
				!before each row pass, randomise the column selection
				call randomised_list(column_randomised)
				
				do column_select=1,columns
					!randomise the column selection
					column_number_2=column_randomised(column_select)

					!set the prospective transition value random multipliers
					call RANDOM_NUMBER(hope)
					call RANDOM_NUMBER(fear)
					!use the distance between the blood channels accordingly
					dist=sqrt((float(row_num-row_number_2)**2)+(float(column_num-column_number_2)**2))
					transition_combine=dist/(sigmoid(blood(column_number_2,row_number_2),"forward",1.0,1.0,0.0,0.0)*gradient)
					transition_square=transition_combine**2
					if (transition_square>50.0) then
						transition_normal=0.0
					else
						transition_normal=exp(-1.0*transition_square)
					end if
					transition=transition_normal*blood(column_number_2,row_number_2)	
					
					!don't act on yo-self
					if ((column_number_2/=column_num) .and. (row_number_2/=row_num)) then

					
						!check if the operation will drain more than the origin channel has
						if (transition<blood(column_number_2,row_number_2)) then
									
							!the equation below is: blood(entry holding data for this position) = blood(entry holding data for this position) + amount of data from the z channel
							blood(column_num,row_num)=blood(column_num,row_num)+transition

							!this takes away the transition amount of data, transferred to the current neuron, from the z channel
							blood(column_number_2,row_number_2)=blood(column_number_2,row_number_2)-transition
						
							!otherwise, drain channel dry
						else if (transition>=blood(column_number_2,row_number_2)) then
							!all of the data from the z neuron is taken
							blood(column_num,row_num)=blood(column_num,row_num)+blood(column_number_2,row_number_2)
						
							!this takes away all the data, transferred to the current channel, from the z channel
							blood(column_number_2,row_number_2)=0.0
						end if
					
					end if
					
				end do
			end do
		end do
	end do

end subroutine blood_mover





!This subroutine Initialise the network - ones for all weights only. If zero, that connection will appear as closed
!data is setup to flow to the bottom
subroutine initialiser(thought,response,volume,response_socket)

	integer,dimension(*) :: response(:)
	integer :: row_number, column_number, path_from, path_to, paths, response_columns
	integer :: rows, columns, info_ports,blood_rows,response_socket
	real :: volume
	type(mind) :: thought
	
	rows=size(thought%brain_weight(1,1,1,:)); columns=size(thought%brain_weight(1,1,:,1)) 
	info_ports=size(thought%brain_status(:,1,1)); paths=size(thought%brain_weight(1,:,1,1))
	blood_rows=size(thought%blood(1,:))
	response_columns=size(response)
	
	!set the initial brain values first
	do row_number=1,rows
		do column_number=1,columns
		
			!zero out brain_status
			thought%brain_status(1,column_number,row_number)=0
			thought%brain_status(2,column_number,row_number)=0	
					
			do path_from=1,paths
				do path_to=1,paths
				
					
					!set up the ones and zeroes
					!zero for any weight directing data off the top if the top isn't open
					if (point_to_neuron(column_number,row_number,path_to,"row")==0) then
						
						thought%brain_weight(path_to,path_from,column_number,row_number)=0.	
									
					!or bottom
					!note, path to the response array is set up here
					else if (point_to_neuron(column_number,row_number,path_to,"row")==rows+1) then
							
						if ((plugin(point_to_neuron(column_number,row_number,path_to,"column"),&
							response_socket,response_columns,"array")>=1) .and. &
							(plugin(point_to_neuron(column_number,row_number,path_to,"column"),&
							response_socket,response_columns,"array")<=response_columns)) then
				
							thought%brain_weight(path_to,path_from,column_number,row_number)=1.
						
						else
						
							thought%brain_weight(path_to,path_from,column_number,row_number)=0.	
						
						end if
						
					!zero for any weight diracting data off the left if it isn't open
					else if (point_to_neuron(column_number,row_number,path_to,"column")==0) then
						
						thought%brain_weight(path_to,path_from,column_number,row_number)=0.	
						
					!or right
					else if (point_to_neuron(column_number,row_number,path_to,"column")==columns+1) then
						
							thought%brain_weight(path_to,path_from,column_number,row_number)=0.	
			
					!otherwise, all weights start off at one
					else
						thought%brain_weight(path_to,path_from,column_number,row_number)=1.
					end if
					
					!activate the if statement below to stop upwards movement in the network
					!if ((path_to==1) .or. (path_to==2) .or. (path_to==3)) then
					!	thought%brain_weight(path_to,path_from,column_number,row_number)=0.
					!end if
					
				end do
			end do
		end do
	end do
	
	!setting up blood network
	do row_number=1,blood_rows
		do column_number=1,columns
			
			thought%blood(column_number,row_number)=(float(row_number)/float(blood_rows))*(volume*2.0)
				
		end do
	end do	
	
	!zero out neurochem
	thought%neurochem=0
	
	!set up empty response array 
	do column_number=1,size(response)
		response(column_number)=0
	end do
	
end subroutine initialiser
	
	








	

!!!!!!!!!!!!!!!!!!!
!!weight handling!!
!!!!!!!!!!!!!!!!!!!





!This subroutine takes the neurochem matricies saved in each network and the output of the motivate network and applies a scaling factor to the weights of each network
!first argument must be the network and the second argument must be the multiplier from the motive network (oddsey)
subroutine animus(meaning,oddyseus)

	type(mind) :: meaning
	integer :: oddyseus, row, column, to, from, ladder

	!add to weights based on the neurochem at that node and the scaling 
	do row=1,size(meaning%brain_weight(1,1,1,:))
		do column=1,size(meaning%brain_weight(1,1,:,1))
			do ladder=1,size(meaning%neurochem(1,:,1,1))
			
				!for each rung in the neurochem ladder, establish which weight is to be altered
				from=meaning%neurochem(1,ladder,column,row)
				to=meaning%neurochem(2,ladder,column,row)
				
				!exclude ladder rungs that haven't been set yet
				if ((to/=0) .and. (from/=0)) then 
					!exclude intentionally zeroed out weights
					if (meaning%brain_weight(to,from,column,row)/=0.0) then
					
						!add the weight
						!the lower the weight is on the ladder, the smaller the modulation
						meaning%brain_weight(to,from,column,row)=meaning%brain_weight(to,from,column,row)+&
							(size(meaning%neurochem(1,:,1,1))+1-ladder)*oddyseus
							
						!if weight manipulation reduces weight below 1.0, make it 1.0
						if (meaning%brain_weight(to,from,column,row)<1.0) then
							meaning%brain_weight(to,from,column,row)=1.0
						end if
						 
					end if
				end if	
				
			end do
		end do
	end do

end subroutine






!This subroutine controls the weight reductions per weight per action
subroutine weight_reducer(weights)

	real,dimension(*) :: weights(:,:,:,:)
	integer :: from,to,connections,column,row,row_total,column_total
	real,parameter :: first_height=1.0-(27.825/34.0), first_gradient=27.825/34.0 !1st stage linear parameters
	real,parameter :: height=-3.3, gradient=1.0 !2nd stage linear parameters
	real,parameter :: period=2.3,amplitude=-2.4 !sinusoid parameters
	real,parameter :: second_period_inverse=10.0,second_amplitude=0.0008,second_sin_shift=300.0 !2nd sinusoid, 2nd stage, parameters
	real,parameter :: normal_height=4.3,normal_distance=2.6,normal_width=5.205 !gaussian parameters
	real :: normalise,normalise_exp
	real,parameter :: overload=5.0
	
	connections=size(weights(:,1,1,1))
	row_total=size(weights(1,1,1,:))
	column_total=size(weights(1,1,:,1))

	do row=1,row_total
		do column=1,column_total
			do from=1, connections
				do to=1, connections
					if (weights(to,from,column,row)>1.) then
						!between 1 and 35, scaling is linear
						if (weights(to,from,column,row)<=35.0) then
							weights(to,from,column,row)=first_gradient*weights(to,from,column,row)+first_height
						!between 35 and 1050, scaling is a non linear function
						else if ((weights(to,from,column,row)>35.0) .and. (weights(to,from,column,row)<1050.0)) then
							!weight is scaled down by a gaussian (smalll numbers), plus a linear (general shape), plus a quadratic (limiter), plus a sinusoid (variation)
							!range within the effective domain (1,big) must stay below the y=x line and above the y=1 line 
							normalise=(((weights(to,from,column,row)-normal_distance)/normal_width)**2)
							if (normalise>50.0) then
								weights(to,from,column,row)=amplitude*sin(0.1*period*weights(to,from,column,row))+&
									gradient*weights(to,from,column,row)+height-&
									second_amplitude*sin((weights(to,from,column,row)+second_sin_shift)/second_period_inverse)
							else
								weights(to,from,column,row)=amplitude*sin(0.1*period*weights(to,from,column,row))+&
									gradient*weights(to,from,column,row)+height-&
									second_amplitude*sin((weights(to,from,column,row)+second_sin_shift)/second_period_inverse)+&
									normal_height*exp(-1.0*(((weights(to,from,column,row)-normal_distance)/normal_width)**2))
							end if
						!if weight is between 1050 and 1000000, just do a constant reduction
						else if ((weights(to,from,column,row)>1050.0) .and. (weights(to,from,column,row)<=1000000.0)) then
							weights(to,from,column,row)=weights(to,from,column,row)-overload
						!limit weight to 1000000
						else
							weights(to,from,column,row)=1000000.0
						end if
					end if
				end do
			end do
		end do
	end do

end subroutine weight_reducer
	
	
	

	

	
end module welcome_to_dying
