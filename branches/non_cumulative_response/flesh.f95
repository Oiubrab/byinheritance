module flesh
implicit none
contains

!each function or subroutine on a rung relies on functions or subroutines on the rung before it



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
	print'(A14,I2,A5,I2,A7,F5.2,A4)',"time elapsed =",t_hr,' hrs, ',t_min,' mins, ',t_sec,' sec'

end subroutine print_interval






!returns a list of sequential numbers up to a length defined by the array input, in a list where the order of the numbers has been randomised
!master_killer is the allocatable array argument and world_eater is the array of known size
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






!this function takes in row/column coordinates and returns the data position of the coordinates
function self_pos(j,i,maximum_row_size) result(z)
	integer,intent(in) :: j,i,maximum_row_size
	integer :: z

	z=((j-1)*maximum_row_size+i)

end function self_pos






!takes in a neuron position along the matrix (j,i,z) with a single number and gives it's position in row/column format
!return either the row, column depending on the opt_pos argument
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






!this function either applies a sigmoid (forward) or inverse sigmoid (reverse) function depending on flow variable
!one can also range strectch with range_stretch and domain stretch with domain_stretch (optional)
!note, inverse sigmoid only goes up to 16*range_stretch
function sigmoid(insig,flow,range_stretch,domain_stretch) result(outsig)
	
	real,intent(in) :: insig
	real,intent(in),optional :: range_stretch,domain_stretch
	real :: outsig
	character(len=*) :: flow

	!domain_stretch and range_stretch defined
	if (present(domain_stretch) .and. present(range_stretch)) then
		!sigmoid
		if (flow=="forward") then
			outsig=range_stretch/(1.+exp(-(1./domain_stretch)*insig))
		!inverse sigmoid
		else if (flow=="reverse") then
			outsig=-range_stretch*log((domain_stretch/insig)-1.)
			if ((1./outsig)==0.) then
				outsig=16.
			end if
		end if

	!domain_stretch defined
	else if (present(domain_stretch)) then
		!sigmoid
		if (flow=="forward") then
			outsig=1./(1.+exp(-(1./domain_stretch)*insig))
		!inverse sigmoid
		else if (flow=="reverse") then
			outsig=-1.*log((domain_stretch/insig)-1.)
			if ((1./outsig)==0.) then
				outsig=16.
			end if	
		end if

	!range_stretch defined
	else if (present(range_stretch)) then
		!sigmoid
		if (flow=="forward") then
			outsig=range_stretch/(1.+exp(-(1./1.)*insig))
		!inverse sigmoid
		else if (flow=="reverse") then
			outsig=-range_stretch*log((1./insig)-1.)
			if ((1./outsig)==0.) then
				outsig=16.
			end if	
		end if

	!unity sigmoid
	else
		!sigmoid
		if (flow=="forward") then
			outsig=1./(1.+exp(-insig))
		!inverse sigmoid
		else if (flow=="reverse") then
			outsig=-log((1./insig)-1.)
			if ((1./outsig)==0.) then
				outsig=16.
			end if
		end if
	end if

end function sigmoid





!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        rung two          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!





!this subroutine tranfers data between neurons, with transfer depending on the relative weights between neurons and random factors
subroutine neuron_fire(emerge,f,u,k,j,i,z,transition_list)

	real :: fate,fear,hope,transition,distil,dist
	real,dimension(*),intent(inout) :: emerge(:,:,:)
	real,dimension(*),intent(inout) :: transition_list(:)
	integer :: f,u,k,j,i,z
	

	!here the neuron data transition is operated
				
	!set the prospective transitionn value random multipliers
	call RANDOM_NUMBER(hope)
	call RANDOM_NUMBER(fear)
	call RANDOM_NUMBER(fate)
	!use the distance between the neurons and weight accordingly
	dist=sqrt((real(f-j)**2)+(real(u-i)**2))
	distil=exp(-(dist*(sigmoid(emerge(j,i,k),"forward")**(-1.)))**2)
	!data element of the z neuron * distance * sigmoid goverened by weights and random numbers
	transition=emerge(f,u,z)*distil*sigmoid((hope*emerge(j,i,z)-fear*emerge(f,u,k)),"forward")


	!print'(F0.4,F0.4,F0.4,F0.4,I2,I2,I2,I2)',emerge(f,u,z),(1-emerge(f,u,k)),emerge(j,i,z),distil,j,i,f,u
				

	!check if the operation will drain more than the origin neuron has
	if (transition<emerge(f,u,z)) then
				
		!the equation below is: emerge(entry holding data for this position) = emerge(entry holding data for this position) + amount of data from the z neuron
		emerge(j,i,k)=emerge(j,i,k)+transition

		!this takes away the transition amount of data, transferred to the current neuron, from the z neuron
		emerge(f,u,z)=emerge(f,u,z)-transition
	
		!otherwise, drain neuron dry (further neuron death algorithm to come)
	else if (transition>=emerge(f,u,z)) then
		!all of the data from the z neuron is taken
		emerge(j,i,k)=emerge(j,i,k)+emerge(f,u,z)
	
		!this takes away all the data, transferred to the current neuron, from the z neuron
		emerge(f,u,z)=0.0
	end if
				
	!add the transition to the list for weight altering
	transition_list(z)=transition

end subroutine neuron_fire





!this subroutine updates the weights for a neuron after it has pulled itself off
subroutine weight_change(emerge,j,i,k,transition_list)

	real,dimension(*),intent(inout) :: emerge(:,:,:)
	real,dimension(*),intent(inout) :: transition_list(:)
	real :: hold_unsig
	integer,intent(in) :: j,i,k
	integer :: z

	do z=1,size(emerge(1,1,:))
				if (k/=z) then
					hold_unsig=transition_list(z)+sigmoid(emerge(j,i,z),"reverse")
					emerge(j,i,z)=sigmoid(hold_unsig,"forward")
				end if
	end do

end subroutine weight_change





end module flesh
