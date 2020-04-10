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
function self_pos(row,column,maximum_row_size) result(z)
	integer,intent(in) :: row,column,maximum_row_size
	integer :: z

	z=((row-1)*maximum_row_size+column)

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
subroutine neuron_pre_fire(brain,brain_freeze,j,i,maximum_columns,maximum_rows)

	real :: fuck
	integer,dimension(*),intent(inout) :: brain(:,:,:)
	integer,dimension(*),intent(inout) :: brain_freeze(:,:)
	integer,intent(in) :: j,i,maximum_columns,maximum_rows
	integer :: k

	!first case is anything off of the border

	if (((i/=1) .and. (i/=maximum_rows)) .and. ((j/=1) .and. (j/=maximum_columns))) then

		
		call RANDOM_NUMBER(fuck)
		if (fuck<0.125) then
			brain_freeze(j,i)=self_pos(i-1,j-1,maximum_columns)
		else if (fuck<0.25) then
			brain_freeze(j,i)=self_pos(i-1,j,maximum_columns)
		else if (fuck<0.375) then
			brain_freeze(j,i)=self_pos(i-1,j+1,maximum_columns)
		else if (fuck<0.5) then
			brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)					
		else if (fuck<0.625) then
			brain_freeze(j,i)=self_pos(i,j+1,maximum_columns)
		else if (fuck<0.75) then
			brain_freeze(j,i)=self_pos(i+1,j-1,maximum_columns)
		else if (fuck<0.875) then
			brain_freeze(j,i)=self_pos(i+1,j,maximum_columns)
		else
			brain_freeze(j,i)=self_pos(i+1,j+1,maximum_columns)
		end if
		!print*,"case1",i,j,brain_freeze(j,i)


	! first set of cases - on the border of the matrix but not in the corner

	else if ((i==1) .and. ((j/=1) .and. (j/=maximum_columns))) then
		
		
		call RANDOM_NUMBER(fuck)
		if (fuck<0.2) then
			brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)
		else if (fuck<0.4) then
			brain_freeze(j,i)=self_pos(i,j+1,maximum_columns)
		else if (fuck<0.6) then
			brain_freeze(j,i)=self_pos(i+1,j-1,maximum_columns)
		else if (fuck<0.8) then
			brain_freeze(j,i)=self_pos(i+1,j,maximum_columns)				
		else
			brain_freeze(j,i)=self_pos(i+1,j+1,maximum_columns)
		end if
		!print*,"case2",i,j,brain_freeze(j,i)
	else if ((i==maximum_rows) .and. ((j/=1) .and. (j/=maximum_columns))) then
		
		
		call RANDOM_NUMBER(fuck)
		if (fuck<0.2) then
			brain_freeze(j,i)=self_pos(i-1,j-1,maximum_columns)
		else if (fuck<0.4) then
			brain_freeze(j,i)=self_pos(i-1,j,maximum_columns)
		else if (fuck<0.6) then
			brain_freeze(j,i)=self_pos(i-1,j+1,maximum_columns)
		else if (fuck<0.8) then
			brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)				
		else
			brain_freeze(j,i)=self_pos(i,j+1,maximum_columns)
		end if
		!print*,"case3",i,j,brain_freeze(j,i)
	else if (((i/=1) .and. (i/=maximum_columns)) .and. (j==1)) then
		
		
		call RANDOM_NUMBER(fuck)
		if (fuck<0.2) then
			brain_freeze(j,i)=self_pos(i-1,j,maximum_columns)
		else if (fuck<0.4) then
			brain_freeze(j,i)=self_pos(i-1,j+1,maximum_columns)
		else if (fuck<0.6) then
			brain_freeze(j,i)=self_pos(i,j+1,maximum_columns)
		else if (fuck<0.8) then
			brain_freeze(j,i)=self_pos(i+1,j,maximum_columns)					
		else
			brain_freeze(j,i)=self_pos(i+1,j+1,maximum_columns)
		end if
		!print*,"case4",i,j,brain_freeze(j,i)
	else if (((i/=1) .and. (i/=maximum_columns)) .and. (j==maximum_columns)) then
		
		
		call RANDOM_NUMBER(fuck)
		if (fuck<0.2) then
			brain_freeze(j,i)=self_pos(i-1,j-1,maximum_columns)
		else if (fuck<0.4) then
			brain_freeze(j,i)=self_pos(i-1,j,maximum_columns)
		else if (fuck<0.6) then
			brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)
		else if (fuck<0.8) then
			brain_freeze(j,i)=self_pos(i+1,j-1,maximum_columns)				
		else
			brain_freeze(j,i)=self_pos(i+1,j,maximum_columns)
		end if
		!print*,"case5",i,j,brain_freeze(j,i)


		!second set of cases - in the corner of the matrix

	else if ((i==1) .and. (j==1)) then
		
		
		call RANDOM_NUMBER(fuck)
		if (fuck<1.0/3.0) then
			brain_freeze(j,i)=self_pos(i,j+1,maximum_columns)
		else if (fuck<2.0/3.0) then
			brain_freeze(j,i)=self_pos(i+1,j,maximum_columns)			
		else
			brain_freeze(j,i)=self_pos(i+1,j+1,maximum_columns)
		end if
		!print*,"case6",i,j,brain_freeze(j,i)
	else if ((i==1) .and. (j==maximum_columns)) then
		
		
		call RANDOM_NUMBER(fuck)
		if (fuck<1.0/3.0) then
			brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)
		else if (fuck<2.0/3.0) then
			brain_freeze(j,i)=self_pos(i+1,j-1,maximum_columns)				
		else
			brain_freeze(j,i)=self_pos(i+1,j,maximum_columns)
		end if
		!print*,"case7",i,j,brain_freeze(j,i)
	else if ((i==maximum_rows) .and. (j==1)) then
		
		
		call RANDOM_NUMBER(fuck)
		if (fuck<1.0/3.0) then
			brain_freeze(j,i)=self_pos(i-1,j,maximum_columns)
		else if (fuck<2.0/3.0) then
			brain_freeze(j,i)=self_pos(i-1,j+1,maximum_columns)			
		else
			brain_freeze(j,i)=self_pos(i,j+1,maximum_columns)
		end if
		!print*,"case8",i,j,brain_freeze(j,i)
	else if ((i==maximum_rows) .and. (j==maximum_columns)) then
		
		
		call RANDOM_NUMBER(fuck)
		if (fuck<1.0/3.0) then
			brain_freeze(j,i)=self_pos(i-1,j-1,maximum_columns)
		else if (fuck<2.0/3.0) then
			brain_freeze(j,i)=self_pos(i-1,j,maximum_columns)			
		else
			brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)
		end if
		!print*,"case9",i,j,brain_freeze(j,i)

	end if

end subroutine neuron_pre_fire


end module flesh
