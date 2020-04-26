module discrete_flesh
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






!takes in a neuron position along the matrix (j,i,z) with a single number and gives it's position in (column,row) format
function point_pos_matrix(z_point,high) result(poster)
	integer,intent(in) :: z_point, high
	integer,dimension(2) :: poster

	!give options for the row or column

	if (z_point<=high) then
		poster(2)=1
	else
		poster(2)=((z_point-1)/high)+1	!this is the row position of the current matrix element represented by z_point
	end if

	poster(1)=z_point-((z_point-1)/high)*high	!this is the column position of the current matrix element represented by z


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
subroutine selector(brain_select,brain_freeze,brain,j,i)

	integer,dimension(*),intent(in) :: brain(:,:,:)
	integer,dimension(*),intent(inout) :: brain_freeze(:,:)
	integer,dimension(*),intent(in) :: brain_select(:)
	real,allocatable :: rungs(:)
	real :: increment, fuck
	integer,intent(in) :: i,j
	integer :: n,maximum_columns

	!set maximum
	maximum_columns=size(brain(1,:,1))

	!number of rungs must equal the number of possible neuron selections
	allocate(rungs(1:size(brain_select)))

	!base incrementation of the rungs must be monotonic
	increment=1/float(size(brain_select))

	call random_number(fuck)

	!set the rungs - ranges for each selection
	rungs(1)=increment+float(brain(brain_select(1),j,i))
	do n=2,size(brain_select)
		rungs(n)=rungs(n-1)+increment+brain(brain_select(n),j,i)
	end do

	!scale the fuck to be the same range as the rungs set
	fuck=fuck*rungs(size(rungs))
	
	!place the chosen pointer in the brain_freeze j,i position
	do n=1,size(brain_select)
		if (fuck<rungs(n)) then
			brain_freeze(j,i)=brain_select(n)
			exit
		end if
	end do

	!just in case the highest number is generated
	if (fuck==rungs(size(rungs))) then
		brain_freeze(j,i)=brain_select(size(brain_select))
	end if

end subroutine selector





!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        rung two          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!




!this subroutine takes the mapping of the data transitions (brain_freeze) and enacts those transitions
subroutine reflect(brain,brain_freeze,valve_selector)

	integer,dimension(*),intent(inout) :: brain(:,:,:)
	integer,dimension(*),intent(inout) :: brain_freeze(:,:)	
	integer :: i,j,maximum_columns,maximum_rows
	integer,dimension(2) :: j_i
	character(len=*) :: valve_selector

	!set maximums
	maximum_columns=size(brain(1,:,1))
	maximum_rows=size(brain(1,1,:))

	!select which sides of the matrix are open to data movement

	if (valve_selector=="closed") then

		do i=1,size(brain(1,1,:))
			do j=1,size(brain(1,:,1))
				!if there is a transition in brain_freeze:
				if (brain_freeze(j,i)/=0) then
					!remove the data from the entry it is currently inhabiting
					brain(self_pos(i,j,maximum_columns),j,i)=brain(self_pos(i,j,maximum_columns),j,i)-1
					!add the data to the target entry
					j_i=point_pos_matrix(brain_freeze(j,i),maximum_columns)
					brain(brain_freeze(j,i),j_i(1),j_i(2))=brain(brain_freeze(j,i),j_i(1),j_i(2))+1
					!add to the corresponding probability variable
					brain(brain_freeze(j,i),j,i)=brain(brain_freeze(j,i),j,i)+2
				end if
			end do
		end do

	else if (valve_selector=="left_open") then

		do i=1,size(brain(1,1,:))
			do j=1,size(brain(1,:,1))
				
				!normal data dransfer
				if ((brain_freeze(j,i)>0) .and. (brain_freeze(j,i)<(maximum_columns*maximum_rows+1))) then
					!remove the data from the entry it is currently inhabiting
					brain(self_pos(i,j,maximum_columns),j,i)=brain(self_pos(i,j,maximum_columns),j,i)-1
					!add the data to the target entry
					j_i=point_pos_matrix(brain_freeze(j,i),maximum_columns)
					brain(brain_freeze(j,i),j_i(1),j_i(2))=brain(brain_freeze(j,i),j_i(1),j_i(2))+1
				!left side data transfer
				else if (brain_freeze(j,i)>(maximum_columns*maximum_rows)) then
					brain(self_pos(i,j,maximum_columns),j,i)=brain(self_pos(i,j,maximum_columns),j,i)-1

				end if

			end do
		end do

	end if

end subroutine reflect






!this subroutine tranfers data between neurons, with transfer depending on the relative weights between neurons and random factors
subroutine neuron_pre_fire(brain,brain_freeze,j_i,valve_selector)

	real :: fuck
	integer,dimension(*),intent(inout) :: brain(:,:,:)
	integer,dimension(*),intent(inout) :: brain_freeze(:,:)
	integer,dimension(2),intent(in) :: j_i
	integer :: maximum_columns,maximum_rows,j,i
	integer,allocatable :: brain_select(:)
	character(len=*) :: valve_selector

	!set maximums and position
	maximum_columns=size(brain(1,:,1))
	maximum_rows=size(brain(1,1,:))
	j=j_i(1)
	i=j_i(2)

	call random_number(fuck)

	!first case is anything off of the border

	if (valve_selector=="closed") then

		if (((i/=1) .and. (i/=maximum_rows)) .and. ((j/=1) .and. (j/=maximum_columns))) then

			allocate(brain_select(1:8))
			brain_select=[self_pos(i-1,j-1,maximum_columns),self_pos(i-1,j,maximum_columns),&
				self_pos(i-1,j+1,maximum_columns),self_pos(i,j-1,maximum_columns),&
				self_pos(i,j+1,maximum_columns),self_pos(i+1,j-1,maximum_columns),&
				self_pos(i+1,j,maximum_columns),self_pos(i+1,j+1,maximum_columns)]
			!print*,"everywhere",i,j,brain_freeze(j,i)


		! first set of cases - on the border of the matrix but not in the corner

		else if ((i==1) .and. ((j/=1) .and. (j/=maximum_columns))) then
			
			allocate(brain_select(1:5))
			brain_select=[self_pos(i,j-1,maximum_columns),self_pos(i,j+1,maximum_columns),&
				self_pos(i+1,j-1,maximum_columns),self_pos(i+1,j,maximum_columns),&
				self_pos(i+1,j+1,maximum_columns)]
			!print*,"top_side",i,j,brain_freeze(j,i)


		else if ((i==maximum_rows) .and. ((j/=1) .and. (j/=maximum_columns))) then	
			
			allocate(brain_select(1:5))
			brain_select=[self_pos(i-1,j-1,maximum_columns),self_pos(i-1,j,maximum_columns),&
				self_pos(i-1,j+1,maximum_columns),self_pos(i,j-1,maximum_columns),&
				self_pos(i,j+1,maximum_columns)]
			!print*,"bottom_side",i,j,brain_freeze(j,i)


		else if (((i/=maximum_rows) .and. (i/=1)) .and. (j==1)) then
			
			allocate(brain_select(1:5))
			brain_select=[self_pos(i-1,j,maximum_columns),self_pos(i-1,j+1,maximum_columns),&
				self_pos(i,j+1,maximum_columns),self_pos(i+1,j,maximum_columns),&
				self_pos(i+1,j+1,maximum_columns)]
			!print*,"left_side",i,j,brain_freeze(j,i)


		else if ((i/=maximum_rows) .and. ((i/=1)) .and. (j==maximum_columns)) then
						
			allocate(brain_select(1:5))
			brain_select=[self_pos(i-1,j-1,maximum_columns),self_pos(i-1,j,maximum_columns),&
				self_pos(i,j-1,maximum_columns),self_pos(i+1,j-1,maximum_columns),&
				self_pos(i+1,j,maximum_columns)]
			!print*,"right_side",i,j,brain_freeze(j,i)


			!second set of cases - in the corner of the matrix

		else if ((i==1) .and. (j==1)) then
			
			allocate(brain_select(1:3))
			brain_select=[self_pos(i,j+1,maximum_columns),self_pos(i+1,j,maximum_columns),&
				self_pos(i+1,j+1,maximum_columns)]
			!print*,"top_left_corner",i,j,brain_freeze(j,i)


		else if ((i==1) .and. (j==maximum_columns)) then
			
			allocate(brain_select(1:3))
			brain_select=[self_pos(i,j-1,maximum_columns),self_pos(i+1,j-1,maximum_columns),&
				self_pos(i+1,j,maximum_columns)]
			!print*,"top_right_corner",i,j,brain_freeze(j,i)


		else if ((i==maximum_rows) .and. (j==1)) then
			
			allocate(brain_select(1:3))
			brain_select=[self_pos(i-1,j,maximum_columns),self_pos(i-1,j+1,maximum_columns),&
				self_pos(i,j+1,maximum_columns)]	
			!print*,"bottom_left_corner",i,j,brain_freeze(j,i)


		else if ((i==maximum_rows) .and. (j==maximum_columns)) then
			
			allocate(brain_select(1:3))
			brain_select=[self_pos(i-1,j-1,maximum_columns),self_pos(i-1,j,maximum_columns),&
				self_pos(i,j-1,maximum_columns)]
			!print*,"bottom_right_corner",i,j,brain_freeze(j,i)


		end if


	!this gives the data a left side escape
	else if (valve_selector=="left_open") then




		if (((i/=1) .and. (i/=maximum_rows)) .and. (j/=maximum_columns)) then

			
			
			if (fuck<0.125) then
				if (j/=1) then
					brain_freeze(j,i)=self_pos(i-1,j-1,maximum_columns)
				else
					brain_freeze(j,i)=maximum_rows*maximum_columns+i-1
				end if
			else if (fuck<0.25) then
				brain_freeze(j,i)=self_pos(i-1,j,maximum_columns)
			else if (fuck<0.375) then
				brain_freeze(j,i)=self_pos(i-1,j+1,maximum_columns)
			else if (fuck<0.5) then
				if (j/=1) then
					brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)
				else
					brain_freeze(j,i)=maximum_rows*maximum_columns+i
				end if				
			else if (fuck<0.625) then
				brain_freeze(j,i)=self_pos(i,j+1,maximum_columns)
			else if (fuck<0.75) then
				if (j/=1) then				
					brain_freeze(j,i)=self_pos(i+1,j-1,maximum_columns)
				else
					brain_freeze(j,i)=maximum_rows*maximum_columns+i+1
				end if
			else if (fuck<0.875) then
				brain_freeze(j,i)=self_pos(i+1,j,maximum_columns)
			else
				brain_freeze(j,i)=self_pos(i+1,j+1,maximum_columns)
			end if
			!print*,"everywhere",i,j,brain_freeze(j,i)


		! first set of cases - on the border of the matrix but not in the corner

		else if ((i==1) .and. ((j/=1) .and. (j/=maximum_columns))) then
			
			
			
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
			!print*,"top_side",i,j,brain_freeze(j,i)
		else if ((i==maximum_rows) .and. ((j/=1) .and. (j/=maximum_columns))) then
			
			
			
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
			!print*,"bottom_side",i,j,brain_freeze(j,i)

		else if (((i/=1) .and. (i/=maximum_rows)) .and. (j==maximum_columns)) then
			
			
			
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
			!print*,"right_side",i,j,brain_freeze(j,i)


			!second set of cases - in the corner of the matrix

		else if ((i==1) .and. (j==1)) then
			
			
			
			if (fuck<0.2) then
				if (j/=1) then
					brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)
				else
					brain_freeze(j,i)=maximum_columns*maximum_rows+i
				end if
			else if (fuck<0.4) then
				brain_freeze(j,i)=self_pos(i,j+1,maximum_columns)
			else if (fuck<0.6) then
				if (j/=1) then
					brain_freeze(j,i)=self_pos(i+1,j-1,maximum_columns)
				else
					brain_freeze(j,i)=maximum_columns*maximum_rows+i+1
				end if					
			else if (fuck<0.8) then
				brain_freeze(j,i)=self_pos(i+1,j,maximum_columns)				
			else
				brain_freeze(j,i)=self_pos(i+1,j+1,maximum_columns)
			end if
			!print*,"top_side",i,j,brain_freeze(j,i)
		else if ((i==1) .and. (j==maximum_columns)) then
			
			
			
			if (fuck<1.0/3.0) then
				brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)
			else if (fuck<2.0/3.0) then
				brain_freeze(j,i)=self_pos(i+1,j-1,maximum_columns)				
			else
				brain_freeze(j,i)=self_pos(i+1,j,maximum_columns)
			end if
			!print*,"top_right_corner",i,j,brain_freeze(j,i)
		else if ((i==maximum_rows) .and. (j==1)) then
			
			
			
			if (fuck<0.2) then
				if (j/=1) then
					brain_freeze(j,i)=self_pos(i-1,j-1,maximum_columns)
				else
					brain_freeze(j,i)=maximum_columns*maximum_rows+i-1
				end if
			else if (fuck<0.4) then
				brain_freeze(j,i)=self_pos(i-1,j,maximum_columns)
			else if (fuck<0.6) then
				brain_freeze(j,i)=self_pos(i-1,j+1,maximum_columns)
			else if (fuck<0.8) then
				if (j/=1) then
					brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)
				else
					brain_freeze(j,i)=maximum_columns*maximum_rows+i
				end if								
			else
				brain_freeze(j,i)=self_pos(i,j+1,maximum_columns)
			end if
			!print*,"bottom_side",i,j,brain_freeze(j,i)
		else if ((i==maximum_rows) .and. (j==maximum_columns)) then
			
			
			
			if (fuck<1.0/3.0) then
				brain_freeze(j,i)=self_pos(i-1,j-1,maximum_columns)
			else if (fuck<2.0/3.0) then
				brain_freeze(j,i)=self_pos(i-1,j,maximum_columns)			
			else
				brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)
			end if
			!print*,"bottom_right_corner",i,j,brain_freeze(j,i)

		end if


	end if

	call selector(brain_select,brain_freeze,brain,j,i)

end subroutine neuron_pre_fire


end module discrete_flesh
