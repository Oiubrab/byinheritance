module discrete_flesh
use emerge
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
	integer :: row,column,maximum_columns,maximum_rows,bottom_bitch
	integer,dimension(2) :: j_i

	!set maximums
	maximum_columns=size(brain(1,:,1))
	maximum_rows=size(brain(1,1,:))


		do row=1,size(brain(1,1,:))
			do column=1,size(brain(1,:,1))
				!if there is a transition in brain_freeze:
				if (brain_freeze(column,row)/=0) then

					!remove the data from the entry it is currently inhabiting
					brain(self_pos_brain(row,column,maximum_columns),column,row)=brain(self_pos_brain(row,column,maximum_columns),column,row)-1

					!for now, only transition data if it is not heading off the board
					if ((brain_freeze(column,row)>maximum_columns+3) .and. (brain_freeze(column,row)<(maximum_rows+2)*(maximum_columns+2)-&
						(maximum_columns+2)) .and. (mod(brain_freeze(column,row),(maximum_columns+2))/=1) .and. &
						(mod(brain_freeze(column,row),(maximum_columns+2))/=0)) then
						
						!add the data to the target entry
						j_i=point_pos_matrix_brain(brain_freeze(column,row),maximum_columns)
						brain(brain_freeze(column,row),j_i(1),j_i(2))=brain(brain_freeze(column,row),j_i(1),j_i(2))+1
				

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
	
		if (self_pos_brain(row-1,column-1,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos_brain(row-1,column-1,maximum_columns)
			position_counter=position_counter+1
		end if
		
		if (self_pos_brain(row-1,column,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos_brain(row-1,column,maximum_columns)
			position_counter=position_counter+1
		end if	
		
		if (self_pos_brain(row-1,column+1,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos_brain(row-1,column+1,maximum_columns)
			position_counter=position_counter+1
		end if	
		
		if (self_pos_brain(row,column-1,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos_brain(row,column-1,maximum_columns)
			position_counter=position_counter+1
		end if	
		
		if (self_pos_brain(row,column+1,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos_brain(row,column+1,maximum_columns)
			position_counter=position_counter+1
		end if	
		
		if (self_pos_brain(row+1,column-1,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos_brain(row+1,column-1,maximum_columns)
			position_counter=position_counter+1
		end if	
		
		if (self_pos_brain(row+1,column,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos_brain(row+1,column-1,maximum_columns)
			position_counter=position_counter+1
		end if	
		
		if (self_pos_brain(row+1,column+1,maximum_columns)/=brain_freeze_nullify) then
			brain_select(position_counter)=self_pos_brain(row+1,column+1,maximum_columns)
			position_counter=position_counter+1
		end if		
	else
		allocate(brain_select(8))
		brain_select=[self_pos_brain(row-1,column-1,maximum_columns),self_pos_brain(row-1,column,maximum_columns),&
			self_pos_brain(row-1,column+1,maximum_columns),self_pos_brain(row,column-1,maximum_columns),&
			self_pos_brain(row,column+1,maximum_columns),self_pos_brain(row+1,column-1,maximum_columns),&
			self_pos_brain(row+1,column,maximum_columns),self_pos_brain(row+1,column+1,maximum_columns)]
	end if

	call selector(brain_select,brain_freeze,brain,column,row)

end subroutine neuron_pre_fire






!this increases the weights sending data towards a neuron, dependant on heartwork
subroutine infusion(brain,blood,scaling)

	integer,dimension(*),intent(inout) :: brain(:,:,:)
	real,dimension(*),intent(in) :: blood(:,:,:)
	integer :: row,column,k,k_blood
	real,intent(in) :: scaling
	
	do row=1,size(brain(1,1,:))
		do column=1,size(brain(1,:,1))
			!setup the position
			k=self_pos_brain(row,column,size(brain(1,:,1)))
			k_blood=self_pos_blood(row,column,size(brain(1,:,1)))
			!add the blood data to the weights into each neuron
			if ((column/=1) .and. (row/=1)) then
				brain(k,column-1,row-1)=brain(k,column-1,row-1)+int(blood(k_blood,column,row)*(10**3)*scaling)
			end if
			if (row/=1) then
				brain(k,column,row-1)=brain(k,column,row-1)+int(blood(k_blood,column,row)*(10**3)*scaling)
			end if
			if ((column/=size(brain(1,:,1))) .and. (row/=1)) then
				brain(k,column+1,row-1)=brain(k,column+1,row-1)+int(blood(k_blood,column,row)*(10**3)*scaling)
			end if
			if (column/=1) then
				brain(k,column-1,row)=brain(k,column-1,row)+int(blood(k_blood,column,row)*(10**3)*scaling)
			end if
			if (column/=size(brain(1,:,1))) then
				brain(k,column+1,row)=brain(k,column+1,row)+int(blood(k_blood,column,row)*(10**3)*scaling)
			end if
			if ((column/=1) .and. (row/=size(brain(1,1,:)))) then
				brain(k,column-1,row+1)=brain(k,column-1,row+1)+int(blood(k_blood,column,row)*(10**3)*scaling)
			end if
			if (row/=size(brain(1,1,:))) then
				brain(k,column,row+1)=brain(k,column,row+1)+int(blood(k_blood,column,row)*(10**3)*scaling)
			end if
			if ((column/=size(brain(1,:,1))) .and. (row/=size(brain(1,1,:)))) then
				brain(k,column+1,row+1)=brain(k,column+1,row+1)+int(blood(k_blood,column,row)*(10**3)*scaling)
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
			if (brain(self_pos_brain(2,column,maximum_columns),column,1)<boundaries(4)) then
				brain(self_pos_brain(2,column,maximum_columns),column,1)=boundaries(4)
			end if
		else
			!one row down
			do row=1,3
				if (brain(self_pos_brain(2,column-2+row,maximum_columns),column,1)<boundaries(4)) then
					brain(self_pos_brain(2,column-2+row,maximum_columns),column,1)=boundaries(4)
				end if
			end do
			!left and right
			do row=1,3
				if ((brain(self_pos_brain(1,column-2+row,maximum_columns),column,1)<boundaries(4)) .and. (column-2+row/=column)) then
					brain(self_pos_brain(1,column-2+row,maximum_columns),column,1)=boundaries(4)
				end if
			end do
		end if
	end do
	
	!adjust bottom side probabilities
	do column=1,size(brain(1,:,1))
		!in the corners
		if ((column==1) .or. (column==size(brain(1,:,1)))) then	
			if (brain(self_pos_brain(maximum_rows-1,column,maximum_columns),column,maximum_rows)<boundaries(1)) then
				brain(self_pos_brain(maximum_rows-1,column,maximum_columns),column,maximum_rows)=boundaries(1)
			end if
		else
			!one row up
			do row=1,3
				if (brain(self_pos_brain(maximum_rows-1,column-2+row,maximum_columns),column,maximum_rows)<boundaries(1)) then
					brain(self_pos_brain(maximum_rows-1,column-2+row,maximum_columns),column,maximum_rows)=boundaries(1)
				end if
			end do
			!left and right
			do row=1,3
				if ((brain(self_pos_brain(maximum_rows,column-2+row,maximum_columns),column,maximum_rows)<boundaries(1))&
					.and. (column-2+row/=column)) then
					brain(self_pos_brain(maximum_rows,column-2+row,maximum_columns),column,maximum_rows)=boundaries(1)
				end if
			end do
		end if
	end do
	
	!adjust left side probabilities
	do row=1,size(brain(1,1,:))
		!in the corners
		if ((row==1) .or. (row==size(brain(1,:,1)))) then	
			if (brain(self_pos_brain(row,2,maximum_columns),1,row)<boundaries(2)) then
				brain(self_pos_brain(row,2,maximum_columns),1,row)=boundaries(2)
			end if
		else
			!one column right
			do column=1,3
				if (brain(self_pos_brain(row-2+column,2,maximum_columns),1,row)<boundaries(2)) then
					brain(self_pos_brain(row-2+column,2,maximum_columns),1,row)=boundaries(2)
				end if
			end do
			!up and down
			do column=1,3
				if ((brain(self_pos_brain(row-2+column,1,maximum_columns),1,row)<boundaries(2)) .and. (row-2+column/=row)) then
					brain(self_pos_brain(row-2+column,1,maximum_columns),1,row)=boundaries(2)
				end if
			end do
		end if
	end do
	
	!adjust right side probabilities	
	do row=1,size(brain(1,1,:))
		!in the corners
		if ((row==1) .or. (row==size(brain(1,:,1)))) then	
			if (brain(self_pos_brain(row,maximum_columns-1,maximum_columns),maximum_columns,row)<boundaries(3)) then
				brain(self_pos_brain(row,maximum_columns-1,maximum_columns),maximum_columns,row)=boundaries(3)
			end if
		else
			!one column left
			do column=1,3
				if (brain(self_pos_brain(row-2+column,maximum_columns-1,maximum_columns),maximum_columns,row)<boundaries(3)) then
					brain(self_pos_brain(row-2+column,maximum_columns-1,maximum_columns),maximum_columns,row)=boundaries(3)
				end if
			end do
			!up and down
			do column=1,3
				if ((brain(self_pos_brain(row-2+column,maximum_columns,maximum_columns),maximum_columns,row)<boundaries(3))& 
					.and. (row-2+column/=row)) then
					
					brain(self_pos_brain(row-2+column,maximum_columns,maximum_columns),maximum_columns,row)=boundaries(3)
					
				end if
			end do
		end if
	end do
	
end subroutine bondage
	
end module discrete_flesh
