module flesh
implicit none
contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        rung one          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!feed in a start and finish time for a time interval printout in hrs, mins, sec
subroutine print_interval(start,finish)
	real,intent(in) :: start, finish
	real :: t_sec, total_time
	integer :: t_hr, t_min
	
	total_time=finish-start
	t_hr = floor(total_time/3600)
	t_min = (total_time-t_hr*3600)/60
	t_sec = total_time-t_hr*3600-t_min*60
	print'(A14,I2,A5,I2,A7,F4.2,A4)',"time elapsed =",t_hr,' hrs, ',t_min,' mins, ',t_sec,' sec'

end subroutine print_interval

!returns a list of sequential numbers up to a length defined by the array input, in a list where the order of the numbers has been randomised
subroutine randomised_list(master_killer)
	integer,allocatable,intent(inout) :: master_killer(:)
	integer :: roger_explosion, length, pos_hold
	real :: despair
		
	length=size(master_killer)
	!print*,length
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

!takes in a neuron position along the matrix (left to right, up to down) with a single number and gives it's position in row/column format
!return either the row, column
function point_pos_matrix(z_point,high,opt_pos) result(poster)
	integer,intent(in) :: z_point, high
	integer :: poster
	character(len=*),intent(in) :: opt_pos

	!give options for the row or column
	if (opt_pos=="row") then
		if (z_point<=high) then
			poster=1
		else
			poster=((z_point-1)/high)+1	!this is the j position of the current matrix element represented by z_point
		end if
	else if (opt_pos=="column") then
		poster=z_point-((z_point-1)/high)*high	!this is the i position of the current matrix element represented by z
	end if

end function point_pos_matrix

!this subroutine tranfers data between neurons, with transfer depending on the relative weights between neurons and random factors
subroutine neuron_fire(emerge,f,u,k,j,i,z,transition_list)

	real :: fate,fear,hope,transition,distil
	real, allocatable,intent(inout) :: emerge(:,:,:),transition_list(:)
	integer :: f,u,k,j,i,z
	

	!here the neuron data transition is operated
				
	!set the prospective transitionn value random multipliers
	call RANDOM_NUMBER(hope)
	call RANDOM_NUMBER(fear)
	call RANDOM_NUMBER(fate)
	!use the distance between the neurons and weight accordingly
	distil=exp(-((sqrt((real(f-j)**2)+(real(u-i)**2)))**2))
	!data element of the z neuron * 1-weight of the z neuron pointing at the current neuron * weight of the current neuron pointing at the z neuron * random number * distance
	transition=emerge(f,u,z)*(1-emerge(f,u,k))*emerge(j,i,z)*fate*distil*(1./(1.+exp(-(hope*emerge(j,i,z)-fear*emerge(f,u,k)))))


	!print'(F0.0,F0.0,F0.0,F0.0,I2,I2,I2,I2)',emerge(f,u,z),(1-emerge(f,u,k)),emerge(j,i,z),distil,j,i,f,u
				

	!check if the operation will drain more than the origin neuron has
	if (transition<emerge(f,u,z)) then
				
		!the equation below is: emerge(entry holding data for this position) = emerge(entry holding data for this position) + amount of data from the z neuron
		emerge(j,i,k)=emerge(j,i,k)+transition

		!this takes away the transition amount of data, transferred to the current neuron, from the z neuron
		emerge(f,u,z)=emerge(f,u,z)-transition
	
		!otherwise, drain neuron dry (further neuron death algorithm to come)
	else if (transition==emerge(f,u,z)) then
		!all of the data from the z neuron is taken
		emerge(j,i,k)=emerge(j,i,k)+emerge(f,u,z)
	
		!this takes away all the data, transferred to the current neuron, from the z neuron
		emerge(f,u,z)=0.0
	end if
				
	!add the transition to the list for weight altering
	transition_list(z)=transition

end subroutine neuron_fire

end module flesh
