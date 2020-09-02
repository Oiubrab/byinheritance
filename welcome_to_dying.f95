module welcome_to_dying
contains

!this is where the magic happens



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!housekeeping - time and metadata!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!delays in microsecond precision
subroutine delay(time)

	real :: time, starter,finisher
	logical :: past=.false.
	
	call cpu_time(starter)
	do while (past .eqv. .false.)
		call cpu_time(finisher)
		if (finisher>starter+time) then
			past=.true.
		end if
	end do
	
	past=.false.
	
end subroutine delay



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






!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!breaking the fourth wall - array address finding!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!Data Direction Map
!the selection below is represented by the point pointing in the fashion

! 1 2 3
! 4 x 5
! 6 7 8

!where x is the column,row position of the input





!takes in the column, row position of the input neuron and a pointing number. Outputs the column or row of the output neuron depending on rowcolumn
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




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!planting and payoff - initialisation and data moving!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!this function selects the neuron to be targeted and sends data from the current neuron (in column,row) to the targeted neuron
!it also currently handles the increase in weights that correspond to data moving through a specific route 
subroutine selector(brain,column,row,reward)

	integer,dimension(*) :: brain(:,:,:)
	integer,allocatable :: brain_select(:)
	real,allocatable :: rungs(:)
	real :: increment, fuck
	integer,intent(in) :: row,column,reward
	integer :: point, connections, data_pos, origin
	
	!setup amount of data ports, data position and origin label
	connections=int(sqrt(float(size(brain(:,1,1)-2))))
	data_pos=size(brain(:,1,1))
	origin=brain(data_pos-1,column,row)
	
	!brain_select contains all the weights, as directed in the point variable contained in the second last brain entry (data_pos-1)
	allocate(brain_select(connections))
	do point=1,size(brain_select)
		if (brain(data_pos,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))==1) then
			!label destinations that have data already in them with 0 weight
			brain_select(point)=0
		else
			!select the set of weights that correspond to the eight directions, as directed by the data's origin (data_pos-1)
			brain_select(point)=brain(((origin-1)*connections)+point,column,row)
		end if
	end do
	
	!set up rungs array	- number of rungs must equal the number of possible neuron selections
	allocate(rungs(1:size(brain_select)))
	do point=1,size(rungs)
		rungs(point)=0.
	end do
	
	!base incrementation of the rungs must be monotonic
	increment=1/float(size(brain_select))

	call random_number(fuck)

	!set the rungs - ranges for each selection
	!rungs = increment + weight stored in neuron for brain_select position
	!if brain_select(point)==0 then close possibility off
	
	!set the first rung
	if (brain_select(1)==0) then
		rungs(1)=0.0
	else
		rungs(1)=increment+float(brain_select(1))
	end if
	!set the subsequent rungs
	do point=2,size(brain_select)
		if (brain_select(point)==0) then
			rungs(point)=rungs(point-1)
		else
			rungs(point)=rungs(point-1)+increment+float(brain_select(point))
		end if
	end do

	!if there is nowhere for the data to go, it has to stay here. Otherwise, find a new home 
	if (rungs(size(rungs))/=0.0) then
			
		!scale the fuck to be the same range as the rungs set
		fuck=fuck*rungs(size(rungs))
		
		!take the chosen neuron and move the data to it
		do point=1,size(brain_select)
			if (fuck<=rungs(point)) then
				!add to weight selection. Weight add should overcome global reduction
				brain(((origin-1)*connections)+point,column,row)=brain(((origin-1)*connections)+point,column,row)+reward
				!remove data and position indicator from current neuron
				brain(data_pos,column,row)=0
				brain(data_pos-1,column,row)=0
				!add data and position indicator to targeted neuron	
				brain(data_pos,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))=1
				brain(data_pos-1,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))=point_origin(point)
				exit
			end if
		end do
		
	end if

end subroutine selector
	




!This subroutine Initialise the network - ones for all weights only. If zero, that connection will appear as closed 
subroutine initialiser(brain,blood)

	integer,dimension(*) :: brain(:,:,:)
	real,dimension(*) :: blood(:,:,:)
	integer :: row_number, column_number, info_number
	integer :: rows, columns, info_ports
	integer :: modding=8,resultant
	
	rows=size(brain(1,1,:)); columns=size(brain(1,:,1)); info_ports=size(brain(:,1,1))

	do row_number=1,rows
		do column_number=1,columns
			do info_number=1,info_ports
			
				!set up the ones and zeroes
				!zero for the last two entries containing data and origin
				if (info_number>=info_ports-1) then
					brain(info_number,column_number,row_number)=0
				!zero for any weight directing data off the top
				else if (point_to_neuron(column_number,row_number,weight_direction(info_number),"row")==0) then
					brain(info_number,column_number,row_number)=0				
				!or bottom
				else if (point_to_neuron(column_number,row_number,weight_direction(info_number),"row")==rows+1) then
					brain(info_number,column_number,row_number)=0
				!zero for any weight diracting data off the left
				else if (point_to_neuron(column_number,row_number,weight_direction(info_number),"column")==0) then
					brain(info_number,column_number,row_number)=0
				!or right
				else if (point_to_neuron(column_number,row_number,weight_direction(info_number),"column")==columns+1) then
					brain(info_number,column_number,row_number)=0
				!otherwise, all weights start off at one
				else if (info_number<info_ports-1) then
					brain(info_number,column_number,row_number)=1
				end if
				
			end do
		end do
	end do
	
	do row_number=1,rows
		do column_number=1,columns
			do info_number=1,info_ports
			
				blood(info_number,column_number,row_number)=0.1
				
			end do
		end do
	end do	
	
end subroutine initialiser
	
	
	
	
end module welcome_to_dying
