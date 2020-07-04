module discrete_flesh
implicit none
contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Each function or subroutine on a rung relies on functions or subroutines on the rung before it. !!
!! If a function/subroutine cannot function without the subroutines/functions on its rung          !!
!! (explicitly), or on rungs above it, then it should be moved up a rung.                          !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!        rung one          !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





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

	!finds the position in the local matrix
	z=((row-1)*maximum_row_size+column)

	!adds the correction for the matrix buffer
	z=z+(maximum_row_size+2)+row*2-1

end function self_pos






!takes in a neuron position along the matrix (column,row,z) with a single number and gives it's position in (column,row) format
function point_pos_matrix(z_point,high) result(poster)
	integer,intent(in) :: z_point, high
	integer :: z
	integer,dimension(2) :: poster

	!poster(2)=row, poster(1)=column
	poster(2)=(z_point-1)/(high+2)
	poster(1)=z_point-poster(2)*(high+2)-1

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





!this function selects the neuron to be targeted and places its position in brain_freeze
subroutine selector(brain_select,brain_freeze,brain,column,row)

	integer,dimension(*),intent(in) :: brain(:,:,:)
	integer,dimension(*),intent(inout) :: brain_freeze(:,:)
	integer,dimension(*),intent(in) :: brain_select(:)
	real,allocatable :: rungs(:)
	real :: increment, fuck
	integer,intent(in) :: row,column
	integer :: n

	!number of rungs must equal the number of possible neuron selections
	allocate(rungs(1:size(brain_select)))
	do n=1,size(rungs)
		rungs(n)=0.
	end do
	
	!base incrementation of the rungs must be monotonic
	increment=1/float(size(brain_select))

	call random_number(fuck)

	!set the rungs - ranges for each selection
	!rungs = increment + weight stored in neuron for brain_select position
	rungs(1)=increment+float(brain(brain_select(1),column,row))
	do n=2,size(brain_select)
		rungs(n)=rungs(n-1)+increment+float(brain(brain_select(n),column,row))
	end do

	!scale the fuck to be the same range as the rungs set
	fuck=fuck*rungs(size(rungs))
	
	!place the chosen pointer in the brain_freeze column,row position
	do n=1,size(brain_select)
		if (fuck<rungs(n)) then
			brain_freeze(column,row)=brain_select(n)
			exit
		end if
	end do

	!just in case the highest number is generated
	if (fuck==rungs(size(rungs))) then
		brain_freeze(column,row)=brain_select(size(brain_select))
	end if

end subroutine selector





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!        rung two          !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




!this subroutine takes the mapping of the data transitions (brain_freeze) and enacts those transitions
subroutine reflect(brain,brain_freeze,dead)

	integer,dimension(*),intent(inout) :: brain(:,:,:)
	integer,dimension(*),intent(inout) :: brain_freeze(:,:)	
	integer,intent(inout) :: dead
	integer :: row,column,maximum_columns,maximum_rows
	integer,dimension(2) :: j_i

	!set maximums
	maximum_columns=size(brain(1,:,1))
	maximum_rows=size(brain(1,1,:))


		do row=1,size(brain(1,1,:))
			do column=1,size(brain(1,:,1))
				!if there is a transition in brain_freeze:
				if (brain_freeze(column,row)/=0) then

					!remove the data from the entry it is currently inhabiting
					brain(self_pos(row,column,maximum_columns),column,row)=brain(self_pos(row,column,maximum_columns),column,row)-1

					!for now, only transition data if it is not heading off the board
					if ((brain_freeze(column,row)>maximum_columns+3) .and. (brain_freeze(column,row)<(maximum_rows+2)*(maximum_columns+2)-(maximum_columns+2))&
						.and. (mod(brain_freeze(column,row),(maximum_columns+2))/=1) .and. &
						(mod(brain_freeze(column,row),(maximum_columns+2))/=0)) then
						
						!add the data to the target entry
						j_i=point_pos_matrix(brain_freeze(column,row),maximum_columns)
						brain(brain_freeze(column,row),j_i(1),j_i(2))=brain(brain_freeze(column,row),j_i(1),j_i(2))+1

					else
						
						dead=dead+1
						!the data must be transmitted to another address or discarded					

					end if

					!add to the corresponding probability variable
					brain(brain_freeze(column,row),column,row)=brain(brain_freeze(column,row),column,row)+2

				end if
			end do
		end do

end subroutine reflect






!this subroutine tranfers data between neurons, with transfer depending on the relative weights between neurons and random factors
subroutine neuron_pre_fire(brain,brain_freeze,j_i,brain_freeze_nullify)

	real :: fuck
	integer,dimension(*),intent(inout) :: brain(:,:,:)
	integer,dimension(*),intent(inout) :: brain_freeze(:,:)
	integer,dimension(2),intent(in) :: j_i
	integer :: maximum_columns,maximum_rows,column,row,position_counter
	integer,optional :: brain_freeze_nullify
	integer,allocatable :: brain_select(:)

	!set maximums and position
	maximum_columns=size(brain(1,:,1))
	maximum_rows=size(brain(1,1,:))
	column=j_i(1)
	row=j_i(2)

	call random_number(fuck)

	!if this call is from the overlap detector that has detected a conflict, eliminate the option for that conflicting transition
	if (present(brain_freeze_nullify)) then
		allocate(brain_select(7))
	
		position_counter=1
	
		if (self_pos(row-1,column-1,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos(row-1,column-1,maximum_columns)
			position_counter=position_counter+1
		end if
		
		if (self_pos(row-1,column,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos(row-1,column,maximum_columns)
			position_counter=position_counter+1
		end if	
		
		if (self_pos(row-1,column+1,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos(row-1,column+1,maximum_columns)
			position_counter=position_counter+1
		end if	
		
		if (self_pos(row,column-1,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos(row,column-1,maximum_columns)
			position_counter=position_counter+1
		end if	
		
		if (self_pos(row,column+1,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos(row,column+1,maximum_columns)
			position_counter=position_counter+1
		end if	
		
		if (self_pos(row+1,column-1,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos(row+1,column-1,maximum_columns)
			position_counter=position_counter+1
		end if	
		
		if (self_pos(row+1,column,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos(row+1,column-1,maximum_columns)
			position_counter=position_counter+1
		end if	
		
		if (self_pos(row+1,column+1,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos(row+1,column+1,maximum_columns)
			position_counter=position_counter+1
		end if		
	else
		allocate(brain_select(8))
		brain_select=[self_pos(row-1,column-1,maximum_columns),self_pos(row-1,column,maximum_columns),&
			self_pos(row-1,column+1,maximum_columns),self_pos(row,column-1,maximum_columns),&
			self_pos(row,column+1,maximum_columns),self_pos(row+1,column-1,maximum_columns),&
			self_pos(row+1,column,maximum_columns),self_pos(row+1,column+1,maximum_columns)]
	end if

	call selector(brain_select,brain_freeze,brain,column,row)

end subroutine neuron_pre_fire






!this increases the weights sending data towards a neuron, dependant on heartwork
subroutine infusion(brain,blood,scaling)

	integer,dimension(*),intent(inout) :: brain(:,:,:)
	real,dimension(*),intent(in) :: blood(:,:,:)
	integer :: row,column,k,k_adj
	real,intent(in) :: scaling
	
	do row=1,size(brain(1,1,:))
		do column=1,size(brain(1,:,1))
			!setup the position
			k=self_pos(row,column,size(brain(1,:,1)))
			k_adj=k-1-size(blood(1,:,1))-row*2
			!add the blood data to the weights into each neuron
			if ((column/=1) .and. (row/=1)) then
				brain(k,column-1,row-1)=brain(k,column-1,row-1)+int(blood(k_adj,column,row)*(10**3)*scaling)
			end if
			if (row/=1) then
				brain(k,column,row-1)=brain(k,column,row-1)+int(blood(k_adj,column,row)*(10**3)*scaling)
			end if
			if ((column/=size(brain(1,:,1))) .and. (row/=1)) then
				brain(k,column+1,row-1)=brain(k,column+1,row-1)+int(blood(k_adj,column,row)*(10**3)*scaling)
			end if
			if (column/=1) then
				brain(k,column-1,row)=brain(k,column-1,row)+int(blood(k_adj,column,row)*(10**3)*scaling)
			end if
			if (column/=size(brain(1,:,1))) then
				brain(k,column+1,row)=brain(k,column+1,row)+int(blood(k_adj,column,row)*(10**3)*scaling)
			end if
			if ((column/=1) .and. (row/=size(brain(1,1,:)))) then
				brain(k,column-1,row+1)=brain(k,column-1,row+1)+int(blood(k_adj,column,row)*(10**3)*scaling)
			end if
			if (row/=size(brain(1,1,:))) then
				brain(k,column,row+1)=brain(k,column,row+1)+int(blood(k_adj,column,row)*(10**3)*scaling)
			!test case - to see data able to escape
			else
				brain(k+2+size(brain(1,:,1)),column,row)=brain(k+2+size(brain(1,:,1)),column,row)+int(blood(k_adj,column,row)*(10**3)*scaling)
			end if
			if ((column/=size(brain(1,:,1))) .and. (row/=size(brain(1,1,:)))) then
				brain(k,column+1,row+1)=brain(k,column+1,row+1)+int(blood(k_adj,column,row)*(10**3)*scaling)
			end if
			
		end do
	end do

end subroutine infusion






!this is the boundary conditions subroutine, where boundaries are given in (bottom, left, right, top) format
subroutine bondage(brain,boundaries)

	integer,dimension(*),intent(inout) :: brain(:,:,:)
	integer,dimension(4),intent(in) :: boundaries
	integer :: maximum_columns,maximum_rows,column,row
	
	!set maximum columns, rows
	maximum_columns=size(brain(1,:,1))
	maximum_rows=size(brain(1,1,:))
	
	!adjust top side probabilities
	do column=1,size(brain(1,:,1))
		!in the corners
		if ((column==1) .or. (column==size(brain(1,:,1)))) then
			if (brain(self_pos(2,column,maximum_columns),column,1)<boundaries(4)) then
				brain(self_pos(2,column,maximum_columns),column,1)=boundaries(4)
			end if
		else
			!one row down
			do row=1,3
				if (brain(self_pos(2,column-2+row,maximum_columns),column,1)<boundaries(4)) then
					brain(self_pos(2,column-2+row,maximum_columns),column,1)=boundaries(4)
				end if
			end do
			!left and right
			do row=1,3
				if ((brain(self_pos(1,column-2+row,maximum_columns),column,1)<boundaries(4)) .and. (column-2+row/=column)) then
					brain(self_pos(1,column-2+row,maximum_columns),column,1)=boundaries(4)
				end if
			end do
		end if
	end do
	
	!adjust bottom side probabilities
	do column=1,size(brain(1,:,1))
		!in the corners
		if ((column==1) .or. (column==size(brain(1,:,1)))) then	
			if (brain(self_pos(maximum_rows-1,column,maximum_columns),column,maximum_rows)<boundaries(1)) then
				brain(self_pos(maximum_rows-1,column,maximum_columns),column,maximum_rows)=boundaries(1)
			end if
		else
			!one row up
			do row=1,3
				if (brain(self_pos(maximum_rows-1,column-2+row,maximum_columns),column,maximum_rows)<boundaries(1)) then
					brain(self_pos(maximum_rows-1,column-2+row,maximum_columns),column,maximum_rows)=boundaries(1)
				end if
			end do
			!left and right
			do row=1,3
				if ((brain(self_pos(maximum_rows,column-2+row,maximum_columns),column,maximum_rows)<boundaries(1)) .and. (column-2+row/=column)) then
					brain(self_pos(maximum_rows,column-2+row,maximum_columns),column,maximum_rows)=boundaries(1)
				end if
			end do
		end if
	end do
	
	!adjust left side probabilities
	do row=1,size(brain(1,1,:))
		!in the corners
		if ((row==1) .or. (row==size(brain(1,:,1)))) then	
			if (brain(self_pos(row,2,maximum_columns),1,row)<boundaries(2)) then
				brain(self_pos(row,2,maximum_columns),1,row)=boundaries(2)
			end if
		else
			!one column right
			do column=1,3
				if (brain(self_pos(row-2+column,2,maximum_columns),1,row)<boundaries(2)) then
					brain(self_pos(row-2+column,2,maximum_columns),1,row)=boundaries(2)
				end if
			end do
			!up and down
			do column=1,3
				if ((brain(self_pos(row-2+column,1,maximum_columns),1,row)<boundaries(2)) .and. (row-2+column/=row)) then
					brain(self_pos(row-2+column,1,maximum_columns),1,row)=boundaries(2)
				end if
			end do
		end if
	end do
	
	!adjust right side probabilities	
	do row=1,size(brain(1,1,:))
		!in the corners
		if ((row==1) .or. (row==size(brain(1,:,1)))) then	
			if (brain(self_pos(row,maximum_columns-1,maximum_columns),maximum_columns,row)<boundaries(3)) then
				brain(self_pos(row,maximum_columns-1,maximum_columns),maximum_columns,row)=boundaries(3)
			end if
		else
			!one column left
			do column=1,3
				if (brain(self_pos(row-2+column,maximum_columns-1,maximum_columns),maximum_columns,row)<boundaries(3)) then
					brain(self_pos(row-2+column,maximum_columns-1,maximum_columns),maximum_columns,row)=boundaries(3)
				end if
			end do
			!up and down
			do column=1,3
				if ((brain(self_pos(row-2+column,maximum_columns,maximum_columns),maximum_columns,row)<boundaries(3)) .and. (row-2+column/=row)) then
					brain(self_pos(row-2+column,maximum_columns,maximum_columns),maximum_columns,row)=boundaries(3)
				end if
			end do
		end if
	end do
	
end subroutine bondage
	
end module discrete_flesh
