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



subroutine print_network(brain,blood,vision,response)

	integer,dimension(*) :: brain(:,:,:),blood(:,:,:),vision(:),response(:)
	integer :: row_counter,column_counter,rows,columns,info_ports
	!printing
	integer,parameter :: individual_width=2, separation_space=10
	character(len=individual_width) :: data_cha
	character(len=12) :: individual_width_cha,separation_cha
	character(len=17) :: width,width_separation
	character(len=:),allocatable :: print_row
	
	!establish network dimensions
	rows=size(brain(1,1,:)); columns=size(brain(1,:,1)); info_ports=size(brain(:,1,1))
	
	!allocate the printing row with enough space to fit both brain and blood
	allocate(character(columns*2*individual_width+2+separation_space) :: print_row) !allocate the printing variable
	
	!set the width to print for each datum
	write(individual_width_cha,*)individual_width
	!set the width to print for the separation between networks
	write(separation_cha,*)separation_space
	!width for brain/blood
	width="(I"//trim(individual_width_cha)//")"
	!width for separation
	width_separation="(A"//trim(separation_cha)//")"

	!print the vision array first
	do column_counter=1,columns
		write(data_cha,width)vision(column_counter)
		print_row(column_counter*individual_width-(individual_width-1):column_counter*individual_width)=data_cha
	end do
	print*,print_row(1:individual_width*columns)
	print*," "

	!print the brain and blood networks beside eachother
	do row_counter=1,rows
		!this loop now handles printing both the brain and the blood networks, hence the columns*2
		!the columns+1 position is empty and creates a space between the two networks
		do column_counter=1,columns*2+1
			!add brain numbers to the first half of the row
			if (column_counter<=columns) then
				write(data_cha,width)brain(info_ports,column_counter,row_counter)
				print_row(column_counter*individual_width-(individual_width-1):column_counter*individual_width)=data_cha
			!create a break for the two networks
			else if (column_counter==columns+1) then
				print_row(column_counter*individual_width-(individual_width-1):&
					column_counter*individual_width-(individual_width-1)+separation_space)="  "
			!and blood numbers to the second half
			else
				write(data_cha,width)blood(info_ports,column_counter-(columns+1),row_counter)
				print_row(column_counter*individual_width-(individual_width-1)+(separation_space-individual_width):&
					column_counter*individual_width+(separation_space-individual_width))=data_cha
			end if
		end do
		print *,print_row
	end do
	print*," "

	!print the response array last
	do column_counter=1,columns
		write(data_cha,width)response(column_counter)
		print_row(column_counter*individual_width-(individual_width-1):column_counter*individual_width)=data_cha
	end do
	print*,print_row(1:individual_width*columns)
	print*," "

end subroutine print_network


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




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!planting and payoff - initialisation and data moving!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!this function selects the neuron to be targeted and sends data from the current neuron (in column,row) to the targeted neuron
!it also currently handles the increase in weights that correspond to data moving through a specific route 
subroutine selector(blood,brain,column,row,reward,response)

	integer,dimension(*) :: brain(:,:,:), blood(:,:,:)
	integer,allocatable :: brain_select(:), response(:)
	real,allocatable :: rungs(:)
	real :: increment, fuck
	integer,intent(in) :: row,column,reward
	integer :: point, connections, data_pos, origin, tester, columnmax, rowmax
	
	!brain size
	rowmax=size(brain(1,1,:)); columnmax=size(brain(1,:,1))
	
	!setup amount of data ports, data position and origin label
	connections=int(sqrt(float(size(brain(:,1,1)-2)))) !8 for adjacent setup
	data_pos=size(brain(:,1,1))
	origin=brain(data_pos-1,column,row)
	
	!brain_select collects all the weights, as directed in the point variable contained in the second last brain entry (data_pos-1)
	allocate(brain_select(connections))
	do point=1,size(brain_select)
		if ((point_to_neuron(column,row,point,"column")>0) .and. &
			(point_to_neuron(column,row,point,"row")>0) .and. &
			(point_to_neuron(column,row,point,"column")<columnmax+1) .and. &
			(point_to_neuron(column,row,point,"row")<rowmax+1)) then
			
				if (brain(data_pos,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))==1) then
					!label destinations that have data already in them with 0 weight
					brain_select(point)=0
				else
					!select the set of weights that correspond to the eight directions, as directed by the data's origin (data_pos-1)
					brain_select(point)=brain(((origin-1)*connections)+point,column,row)
				end if
				
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
				
				!moving diagnostic
				print*,"Move from:"
				print*,column,row
				print*,"Move to:"
				print*,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row")
				print*," "
			
				!add to weight selection. Weight add should overcome global reduction
				brain(((origin-1)*connections)+point,column,row)=brain(((origin-1)*connections)+point,column,row)+reward
				!remove data and position indicator from current neuron
				brain(data_pos,column,row)=0
				brain(data_pos-1,column,row)=0
				
				!if data is moving off the brain, direct it into the response array
				!data moving off the sides needs to be directed to the correct row
				if ((point_to_neuron(column,row,point,"column")==0) .or. &
					(point_to_neuron(column,row,point,"column")==columnmax+1)) then
					
					response(point_to_neuron(column,row,point,"row"))=1
					
				!data moving of the top/bottom needs to be directed to the correct column
				else if ((point_to_neuron(column,row,point,"row")==0) .or. &
					(point_to_neuron(column,row,point,"row")==rowmax+1)) then
					
					response(point_to_neuron(column,row,point,"column"))=1
					
				!if the data is not moving off the brain, move it normally
				else	
							
					!add data and position indicator to targeted neuron	
					brain(data_pos,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))=1
					brain(data_pos-1,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))=point_origin(point)
					!add something to blood at that position
					blood(data_pos,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))=&
						blood(data_pos,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))+1
				
				end if
				
				exit
				
			end if
		end do
		
	end if

end subroutine selector
	



!this subroutine takes in a neuron from the blood network and moves it around
subroutine blood_mover(blood,column_num,row_num)

	integer,dimension(*) :: blood(:,:,:)
	integer :: column_num,row_num,info_ports,row_number_2,column_number_2
	real :: blood_difference
	integer :: blood_transition,rows,columns
	
	info_ports=size(blood(:,1,1)); rows=size(blood(1,1,:)); columns=size(blood(1,:,1))

	!engage every blood neuron around the neuron in question
	do row_number_2=row_num-1,row_num+1
		do column_number_2=column_num-1,column_num+1
			!the transitions need to act on any adjacent neuron that exists, i.e. not off the side of the network. They also must avoid acting on themselves
			if (((row_number_2/=row_num) .and. (column_number_2/=column_num)) .and. &
				((row_number_2<=rows) .and. (column_number_2<=columns) .and. (row_number_2>0) .and. (column_number_2>0))) then
				
				!calculate the transition to occur between neurons
				!its half the origin neuron times the fraction of the difference between destination neuron and origin neuron, over the origin neuron
				blood_difference=float(abs(blood(info_ports,column_num,row_num)-&
					blood(info_ports,column_number_2,row_number_2)))/float(blood(info_ports,column_num,row_num))
				!print*,blood_difference,abs(blood(info_ports,column_num,row_num)-&
				!	blood(info_ports,column_number_2,row_number_2))
				blood_transition=int(blood(info_ports,column_num,row_num)*(0.5*blood_difference))
				
				!make sure no neuron becomes negative
				if (blood_transition>blood(info_ports,column_num,row_num)-1) then
					blood_transition=blood(info_ports,column_num,row_num)-1
				end if
				
				!make the transition
				blood(info_ports,column_number_2,row_number_2)=blood(info_ports,column_number_2,row_number_2)+blood_transition
				blood(info_ports,column_num,row_num)=blood(info_ports,column_num,row_num)-blood_transition
					
			end if
		end do
	end do

	
end subroutine blood_mover




!This subroutine Initialise the network - ones for all weights only. If zero, that connection will appear as closed
!the openside variable decides which side data will flow out from
subroutine initialiser(brain,blood,vision,response,openside)

	integer,dimension(*) :: brain(:,:,:), blood(:,:,:), vision(:), response(:)
	integer :: row_number, column_number, info_number
	integer :: rows, columns, info_ports
	integer :: modding=8,resultant
	character(len=*) :: openside
	
	rows=size(brain(1,1,:)); columns=size(brain(1,:,1)); info_ports=size(brain(:,1,1))

	do row_number=1,rows
		do column_number=1,columns
			do info_number=1,info_ports
			
				!set up the ones and zeroes
				!zero for the last two entries containing data and origin
				if (info_number>=info_ports-1) then
					brain(info_number,column_number,row_number)=0
				!zero for any weight directing data off the top if the top isn't open
				else if ((point_to_neuron(column_number,row_number,weight_direction(info_number),"row")==0) .and. &
					(openside/="top")) then
					
					brain(info_number,column_number,row_number)=0	
								
				!or bottom
				else if ((point_to_neuron(column_number,row_number,weight_direction(info_number),"row")==rows+1) .and. &
					(openside/="bottom")) then

					brain(info_number,column_number,row_number)=0
					
				!zero for any weight diracting data off the left if it isn't open
				else if ((point_to_neuron(column_number,row_number,weight_direction(info_number),"column")==0) .and. &
					(openside/="left")) then
					
					brain(info_number,column_number,row_number)=0
					
				!or right
				else if ((point_to_neuron(column_number,row_number,weight_direction(info_number),"column")==columns+1) .and. &
					(openside/="right")) then
					
					brain(info_number,column_number,row_number)=0
					
				!otherwise, all weights start off at one
				else if (info_number<info_ports-1) then
					brain(info_number,column_number,row_number)=1
				end if
				
			end do
		end do
	end do
	
	!setting up blood network, below only adds 0.1 to every entry
	do row_number=1,rows
		do column_number=1,columns
			do info_number=1,info_ports
			
				blood(info_number,column_number,row_number)=1
				
			end do
		end do
	end do	

	!set up empty vision array 
	do column_number=1,size(vision)
		vision(column_number)=0
	end do
	
	!set up empty response array 
	do column_number=1,size(vision)
		response(column_number)=0
	end do
	
end subroutine initialiser
	
	
	
	
end module welcome_to_dying
