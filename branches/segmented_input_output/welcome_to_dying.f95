module welcome_to_dying

!define type for brain
type mind
	integer,allocatable :: brain_status(:,:,:)
	real,allocatable :: brain_weight(:,:,:,:)
	real,allocatable :: blood(:,:)
	integer,allocatable :: neurochem(:,:,:)
end type mind

!define type for vision
type eyeball
	integer,allocatable :: sense_action(:,:)
	integer,allocatable :: socket_number(:)
end type eyeball

!define type for response
type responder
	integer,allocatable :: turn(:),speed(:) !turn is on left, speed is on right
	!socket(1) is turn, socket(2) is speed
	integer,dimension(2) :: socket_number
end type responder


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





subroutine print_network(vision,vision_socket,response,response_socket,brain,blood)

	integer,dimension(*) :: brain(:,:,:),vision(:),response(:)
	real,optional,dimension(*) :: blood(:,:)
	integer :: row_counter,column_counter,rows,columns,info_ports,blood_rows,blood_columns,vision_columns,response_columns
	integer,intent(in) :: vision_socket,response_socket
	!printing
	integer,parameter :: individual_width=2, individual_blood=6, separation_space=10
	character(len=individual_width) :: data_cha
	character(len=individual_blood) :: blood_cha
	character(len=12) :: individual_width_cha,separation_cha,individual_blood_cha
	character(len=17) :: width,blood_width,empty_width
	character(len=:),allocatable :: print_row
	
	!establish network dimensions
	rows=size(brain(1,1,:)); columns=size(brain(1,:,1)); info_ports=size(brain(:,1,1))
	!if blood is present, set ther size, otherwise the size is zero
	if (present(blood)) then
		blood_rows=size(blood(1,:)); blood_columns=size(blood(:,1))
	else 
		blood_rows=0; blood_columns=0
	end if	
	!and set the com arrays
	vision_columns=size(vision); response_columns=size(response)
	
	!allocate the printing row with enough space to fit both brain and blood if blood is present
	if (present(blood)) then
		allocate(character((columns*individual_width)+(blood_columns*individual_blood)+2+separation_space) :: print_row) !allocate the printing variable
	else
		allocate(character((columns*individual_width)+2) :: print_row) !allocate the printing variable	
	end if
	
	!set the width to print for each datum
	write(individual_width_cha,*)individual_width
	write(individual_blood_cha,*)individual_blood
	!width for brain/blood
	width="(I"//trim(individual_width_cha)//")"
	empty_width="(A"//trim(individual_width_cha)//")"
	blood_width="(F"//trim(individual_blood_cha)//".2)"

	!print the vision array first
	do column_counter=1,columns
		!print*,plugin(column_counter,vision_socket,vision_columns,"array"),column_counter
		!set up printer to print vision array in the correct columns
		if ((column_counter>=plugin(1,vision_socket,vision_columns,"brain")) .and. &
			(column_counter<=plugin(vision_columns,vision_socket,vision_columns,"brain"))) then
			write(data_cha,width)vision(plugin(column_counter,vision_socket,vision_columns,"array"))
		else
			write(data_cha,empty_width)"  "
		end if
		print_row(column_counter*individual_width-(individual_width-1):column_counter*individual_width)=data_cha
	end do
	print*,print_row(1:individual_width*columns)
	print*," "
	print_row(:)="  "

	!the main brain printing loop
	do row_counter=1,rows
		!this loop now handles printing both the brain and the blood networks, hence the columns*2
		!the columns+1 position is empty and creates a space between the two networks
		do column_counter=1,columns+blood_columns+1
			!add brain numbers to the first half of the row
			if (column_counter<=columns) then
				write(data_cha,width)brain(info_ports,column_counter,row_counter)
				print_row(column_counter*individual_width-(individual_width-1):column_counter*individual_width)=data_cha
			!print the brain and blood networks beside eachother if blood is passed in
			else if (present(blood)) then
				!create a break for the two networks
				if (column_counter==columns+1) then
					print_row(column_counter*individual_width-(individual_width-1):&
						column_counter*individual_width-(individual_width-1)+separation_space)="  "
				!and blood numbers to the second half
				else
					write(blood_cha,blood_width)blood(column_counter-(columns+1),row_counter)
					!from: brain columns + separation + blood column number * blood column width - (blood column width + 1)
					!to: brain columns + separation + blood column number * blood column width
					print_row(columns*individual_width+separation_space+(column_counter-(columns+1))*&
						individual_blood-(individual_blood-1):columns*individual_width+separation_space+&
						(column_counter-(columns+1))*individual_blood)=blood_cha
				end if
			end if
		end do
		print *,print_row
	end do
	print*," "
	print_row(:)="  "

	!print the response and equivalent blood arrays last
	do column_counter=1,columns+blood_columns+1
		!add response numbers to the first half of the row
		if (column_counter<=columns) then
			!place them where the socket is
			if ((column_counter>=plugin(1,response_socket,response_columns,"brain")) .and. &
				(column_counter<=plugin(response_columns,response_socket,response_columns,"brain"))) then
				write(data_cha,width)response(plugin(column_counter,response_socket,response_columns,"array"))
			else
				write(data_cha,empty_width)"  "
			end if
		print_row(column_counter*individual_width-(individual_width-1):column_counter*individual_width)=data_cha
		!print the brain and blood networks beside eachother if blood is passed in
		else if (present(blood)) then
			if (column_counter==columns+1) then
				print_row(column_counter*individual_width-(individual_width-1):&
					column_counter*individual_width-(individual_width-1)+separation_space)="  "
			!and blood numbers to the second half
			else
				write(blood_cha,blood_width)blood(column_counter-(columns+1),blood_rows)
				!from: brain columns + separation + blood column number * blood column width - (blood column width + 1)
				!to: brain columns + separation + blood column number * blood column width
				print_row(columns*individual_width+separation_space+(column_counter-(columns+1))*&
					individual_blood-(individual_blood-1):columns*individual_width+separation_space+&
					(column_counter-(columns+1))*individual_blood)=blood_cha
			end if
		end if
	end do
	print*,print_row
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



!read in the network or write out to a text file
subroutine read_write(think,epoch,moves,vision,direction)
	type(mind) :: think
	integer :: epoch,vision
	character(len=*) :: direction
	integer :: column,row
	
	if (direction=="read") then
	
		!retrieve previous network
		open(unit=1,file="will.txt")
		read(1,*) think%brain_status
		read(1,*) think%brain_weight
		read(1,*) think%blood
		read(1,*) think%neurochem				
		read(1,*) epoch
		read(1,*) moves
		read(1,*) vision
		close(1)
		
	else if (direction=="write") then
	
		!write the networks to file
		open(unit=2,file="will.txt")
		write(2,*) think%brain_status
		write(2,*) think%brain_weight
		write(2,*) think%blood
		write(2,*) think%neurochem		
		write(2,*) epoch
		write(2,*) moves
		write(2,*) vision
		close(2)
	
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


!this subroutine converts given angles, tracing a line from the cat to the mouse, into a position in an input array
!currently, this takes in two 'eye' angles, leading from the left and right side of the cat, and translates them into positions for the left and right array
!not done yet
subroutine angle_to_vision(vision,cat_left_eye,cat_right_eye)

	real,parameter :: pi=4.*asin(1./sqrt(2.))
	real :: select_range,cat_left_eye,cat_right_eye
	real,dimension(2) :: left_range,right_range
	integer :: column_number
	type(eyeball) :: vision

	!these set the angular ranges that the character can see from each eye
	!note: in the game, negative pi is on the left, and positive pi is on the right
	!the whole system is revolved by +pi for mathematical simplicity (might be stupid)
	!so, 0 to pi is left of the character, and pi to 2pi is right of the character
	left_range=[0,pi*1.5]
	right_range=[0.5*pi,2.0*pi]
	!this assignment assumes the eyes are equal length, as there is only one select_range for two eyes
	select_range=(1.5*pi)/float(size(vision%sense_action(:,1)))
	!note, the +pi bit in the if statement converts the cat direction coming in (-pi to pi) into 0 to 2pi
	!left eye
	do column_number=1,size(vision%sense_action(:,1))
	
		if ((select_range*float(column_number)>=(cat_angle+pi)) .and. (select_range*float(column_number-1)<=(cat_angle+pi))) then
			vision%(column_number)=1
			!print*,"help"
		else
			vision(column_number)=0
		end if
		
	end do
	!right eye
	do column_number=1,size(vision%sense_action(:,2))
	
		if ((select_range*float(column_number)>=(cat_angle+pi)) .and. (select_range*float(column_number-1)<=(cat_angle+pi))) then
			vision(column_number)=1
			!print*,"help"
		else
			vision(column_number)=0
		end if
		
	end do

end subroutine angle_to_vision






!this function selects the neuron to be targeted and sends data from the current neuron (in column,row) to the targeted neuron
!it also currently handles the increase in weights that correspond to data moving through a specific route 
subroutine selector(idea,column,row,reward,response,response_socket,printer)

	type(mind) :: idea
	integer,allocatable :: response(:),connection_translation(:)
	real,allocatable :: rungs(:)
	real :: increment, fuck, reward, blood_trans=0.05
	integer,intent(in) :: row,column,response_socket
	integer :: rung, point, connections, data_pos, origin, tester
	integer :: columnmax, rowmax, counter, second_point, response_length
	logical :: printer
	
	!brain size
	rowmax=size(idea%brain_status(1,1,:)); columnmax=size(idea%brain_status(1,:,1)); response_length=size(response)
	
	!setup amount of data ports, data position and origin label
	connections=size(idea%brain_weight(:,1,1,1))
	data_pos=size(idea%brain_status(:,1,1))
	origin=idea%brain_status(data_pos-1,column,row)
	
	!set up rungs array	- number of rungs must equal the number of possible neuron selections
	allocate(rungs(connections))
	do point=1,size(rungs)
		rungs(point)=0.
	end do
	
	!base incrementation of the rungs must be monotonic
	increment=1.0/float(connections)

	call random_number(fuck)

	!setup connection_translation, a randomising position for the rungs - weights selector
	allocate(connection_translation(connections))
	call randomised_list(connection_translation)

	!set the rungs - ranges for each selection
	!rungs = increment + weight stored in neuron
	!if brain_status(point)==0 then close possibility off
	!set the first rung
	
	!set the subsequent rungs
	do rung=1,connections
		!translate rung number to random point variable
		point=connection_translation(rung)
		!no backwards movement
		if (origin==point) then
			if (rung==1) then
				rungs(rung)=0.0
			else
				rungs(rung)=rungs(rung-1)
			end if		
		!directing outside the network and not in a response layer
		else if (idea%brain_weight(point,origin,column,row)==0.0) then
			if (rung==1) then
				rungs(rung)=0.0
			else
				rungs(rung)=rungs(rung-1)
			end if		
		!inside the network
		else if ((point_to_neuron(column,row,point,"column")>=1) .and. (point_to_neuron(column,row,point,"row")>=1) .and. &
			(point_to_neuron(column,row,point,"column")<=columnmax) .and. (point_to_neuron(column,row,point,"row")<=rowmax)) then
			!data in node
			if (idea%brain_status(data_pos,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))==1) then
				if (rung==1) then
					rungs(rung)=0.0
				else
					rungs(rung)=rungs(rung-1)
				end if		
			!open node in network
			else
				if (rung==1) then
					rungs(rung)=increment+idea%brain_weight(point,origin,column,row)*&
						idea%blood(point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))
				else
					rungs(rung)=rungs(rung-1)+increment+idea%brain_weight(point,origin,column,row)*&
						idea%blood(point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))
				end if	
				
			end if
		!response array	
		else
		
			if (rung==1) then
				rungs(rung)=increment+idea%brain_weight(point,origin,column,row)*&
					idea%blood(point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))
			else
				rungs(rung)=rungs(rung-1)+increment+idea%brain_weight(point,origin,column,row)*&
					idea%blood(point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))
			end if	
			
		end if
		
	end do



	if (printer .eqv. .true.) then
		print*,"Maximum Rungs Value, choice value:"
		print*,rungs(size(rungs)),fuck*rungs(size(rungs))
		print*,"Weightings from Direction 1 to 8:"
		print"(F10.2,F10.2,F10.2,F10.2,F10.2,F10.2,F10.2,F10.2)",idea%brain_weight(:,origin,column,row)
		print*,"Rungs from Direction 1 to 8:"
		print"(F10.2,F10.2,F10.2,F10.2,F10.2,F10.2,F10.2,F10.2)",rungs(connection_translation(1)),rungs(connection_translation(2)),&
			rungs(connection_translation(3)),rungs(connection_translation(4)),rungs(connection_translation(5)),&
			rungs(connection_translation(6)),rungs(connection_translation(7)),rungs(connection_translation(8))
	end if

	!if there is nowhere for the data to go, it has to stay here. Otherwise, find a new home 
	if (rungs(size(rungs))/=0.0) then
			
		!scale the fuck to be the same range as the rungs set
		fuck=fuck*rungs(size(rungs))
		
		!take the chosen neuron and move the data to it
		do rung=1,connections
			if (fuck<=rungs(rung)) then
				
				!translate rung into corresponding weight
				point=connection_translation(rung)
				
				!moving diagnostic
				if (printer .eqv. .true.) then
					print*,"Move from:"
					print*,column,row
					print*,"Move to:"
					print*,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row")
					print*," "
				end if
			
				!add to weight selection. Weight add should overcome global reduction
				idea%brain_weight(point,origin,column,row)=idea%brain_weight(point,origin,column,row)+reward
				!remove data and position indicator from current neuron
				idea%brain_status(data_pos,column,row)=0
				idea%brain_status(data_pos-1,column,row)=0
				!add data to neurochem
				idea%neurochem(1,column,row)=origin
				idea%neurochem(2,column,row)=point				
				
				!if data is moving off the brain, direct it into the response array	
				if (point_to_neuron(column,row,point,"row")==rowmax+1) then
					
					response(plugin(point_to_neuron(column,row,point,"column"),response_socket,response_length,"array"))=1
					
				!if the data is not moving off the brain, move it normally
				else	
							
					!add data and position indicator to targeted neuron	
					idea%brain_status(data_pos,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))=1
					idea%brain_status(data_pos-1,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))=&
						point_origin(point)
						
					!add something to blood at that position by taking away blood from channels surrounding it
					counter=0
					do second_point=1,connections
						!first, check if channels around next channel exist
						if ((point_to_neuron(point_to_neuron(column,row,point,"column"),&
							point_to_neuron(column,row,point,"row"),second_point,"column")>1) .and. &
							(point_to_neuron(point_to_neuron(column,row,point,"column"),&
							point_to_neuron(column,row,point,"row"),second_point,"row")>1) .and. &
							(point_to_neuron(point_to_neuron(column,row,point,"column"),&
							point_to_neuron(column,row,point,"row"),second_point,"column")<=columnmax) .and. &
							(point_to_neuron(point_to_neuron(column,row,point,"column"),&
							point_to_neuron(column,row,point,"row"),second_point,"row")<=rowmax)) then
							!then, check if those channels have enough blood to give
							if (idea%blood(point_to_neuron(point_to_neuron(column,row,point,"column"),&
								point_to_neuron(column,row,point,"row"),second_point,"column"),&
								point_to_neuron(point_to_neuron(column,row,point,"column"),&
								point_to_neuron(column,row,point,"row"),second_point,"row"))>blood_trans) then
						
								idea%blood(point_to_neuron(point_to_neuron(column,row,point,"column"),&
									point_to_neuron(column,row,point,"row"),second_point,"column"),&
									point_to_neuron(point_to_neuron(column,row,point,"column"),&
									point_to_neuron(column,row,point,"row"),second_point,"row"))=&
									idea%blood(point_to_neuron(point_to_neuron(column,row,point,"column"),&
									point_to_neuron(column,row,point,"row"),second_point,"column"),&
									point_to_neuron(point_to_neuron(column,row,point,"column"),&
									point_to_neuron(column,row,point,"row"),second_point,"row"))-blood_trans
								
								counter=counter+1
								
							end if
									
						end if
						
					end do	
					!finally, add the blood taken from surrounding channels into the next channel
					idea%blood(point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))=&
						idea%blood(point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))+(blood_trans*float(counter))
				
				end if
				
				exit
				
			end if
		end do
		
	end if

end subroutine selector




!this subroutine takes in a neuron from the blood network and moves it around
subroutine blood_mover(blood,column_num,row_num,gradient)

	real,dimension(*) :: blood(:,:)
	integer,allocatable :: row_randomised(:), column_randomised(:)
	integer :: column_num,row_num,row_select,column_select
	real :: hope, fear, dist, distil, transition
	real :: gradient !controls how quickly blood flows. bigger is faster
	integer :: rows,columns,row_number_2,column_number_2
	
	rows=size(blood(1,:)); columns=size(blood(:,1))

	!make the random arrays as big as columns and rows
	allocate(row_randomised(rows))
	allocate(column_randomised(columns))
	
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

			!set the prospective transitionn value random multipliers
			call RANDOM_NUMBER(hope)
			call RANDOM_NUMBER(fear)
			!use the distance between the blood channels accordingly
			dist=sqrt((float(row_num-row_number_2)**2)+(float(column_num-column_number_2)**2))
			transition=exp(-(dist*(sigmoid(blood(column_number_2,row_number_2),"forward",1.0,1.0,0.0,0.0)**(-1.))/gradient)**2)*&
				blood(column_number_2,row_number_2)	
			
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

end subroutine blood_mover





!This subroutine Initialise the network - ones for all weights only. If zero, that connection will appear as closed
!data is setup to flow to the bottom
subroutine initialiser(thought,response,volume)

	type(responder) :: response
	integer :: row_number, column_number, path_from, path_to, paths, response_columns
	integer :: rows, columns, info_ports,blood_rows
	real :: volume
	type(mind) :: thought
	
	rows=size(thought%brain_weight(1,1,1,:)); columns=size(thought%brain_weight(1,1,:,1)) 
	info_ports=size(thought%brain_status(:,1,1)); paths=size(thought%brain_weight(1,:,1,1))
	blood_rows=size(thought%blood(1,:))
	
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


!this subroutine controls how the neurochem modifies weights to reward certain behaviour
subroutine motivation(neurochemical,weighting,old_look,new_look,centre,effect_up,effect_down,effect_all_around,grad)

	integer,dimension(*) :: neurochemical(:,:,:)
	real,dimension(*) :: weighting(:,:,:,:)
	integer :: column,row,path,effect_plus,effect_minus
	integer :: difference_to_centre,new_difference,old_difference
	real,intent(in) :: effect_up,effect_down,effect_all_around,grad
	integer,intent(in) :: old_look,new_look,centre !note, grad must be less than (centre-1)/2
	
	!zero out all free variables just in case
	difference_to_centre=0
	new_difference=0
	old_difference=0
	
	!calculate the difference from the centrepoint before and after movement
	old_difference=abs(centre-old_look)
	new_difference=abs(centre-new_look)
	difference_to_centre=old_difference-new_difference !pos if closer to centre, neg if further away
	
	!update effects with difference_to_centre values
	effect_plus=effect_up*abs(difference_to_centre)
	effect_minus=effect_down*abs(difference_to_centre)
	
	!note: neurochem(1,:,:) is origin, neurochem(2,:,:) is point
	!weighting(x,:,x,x) is origin, weighting(:,x,x,x) is point
	do row=1,size(neurochemical(1,1,:))	
		do column=1,size(neurochemical(1,:,1))
		
			if (neurochemical(1,column,row)/=0) then	
				!straight reward - closer to centre, higher the reward
				weighting(neurochemical(2,column,row),neurochemical(1,column,row),column,row)=&
					weighting(neurochemical(2,column,row),neurochemical(1,column,row),column,row)+effect_all_around*&
					((float(centre)-(grad*float(new_difference)))/float(centre))	
				!straight punishment - further from centre, higher the punishment
				!ensure connection is not zeroed
				if (weighting(neurochemical(2,column,row),neurochemical(1,column,row),column,row)-effect_all_around*&
					(((grad*float(new_difference))+1.0)/float(centre))>1.0) then
					
					weighting(neurochemical(2,column,row),neurochemical(1,column,row),column,row)=&
						weighting(neurochemical(2,column,row),neurochemical(1,column,row),column,row)-effect_all_around*&
						(((grad*float(new_difference))+1.0)/float(centre))
						
				else
					weighting(neurochemical(2,column,row),neurochemical(1,column,row),column,row)=1.0
				end if
				
				!pleasure and pain: if the response moves vision away from centre (pain), drive the weights closer to unity
				!if the response moves vision towards the centre (pleasure), increase the weight disparity
				!don't act on closed paths
				if (weighting(neurochemical(2,column,row),neurochemical(1,column,row),column,row)/=0.0) then
					!pleasure: the vision datum is being driven closer to centre
					if (difference_to_centre>=1) then
						weighting(neurochemical(2,column,row),neurochemical(1,column,row),column,row)=&
							weighting(neurochemical(2,column,row),neurochemical(1,column,row),column,row)+effect_plus
					!pain: the vision datum is being driven away from the centre
					else if (difference_to_centre<=-1) then
						!subtracting from errant path
						if (weighting(neurochemical(2,column,row),neurochemical(1,column,row),column,row)-effect_minus<1.0) then
							weighting(neurochemical(2,column,row),neurochemical(1,column,row),column,row)=1.0
						else
							weighting(neurochemical(2,column,row),neurochemical(1,column,row),column,row)=&
								weighting(neurochemical(2,column,row),neurochemical(1,column,row),column,row)-effect_minus
						end if
					end if
				end if
					
			end if
		end do
	end do

end subroutine motivation



!This subroutine controls the weight reductions per weight per action
subroutine weight_reducer(weights,column,row,max_weight)

	real,dimension(*) :: weights(:,:,:,:)
	real,intent(in) :: max_weight
	integer :: from,to,connections,column,row
	real,parameter :: first_height=1.0-(27.825/34.0), first_gradient=27.825/34.0 !1st stage linear parameters
	real,parameter :: height=-3.3, gradient=1.0 !2nd stage linear parameters
	real,parameter :: period=2.3,amplitude=-2.4 !sinusoid parameters
	real,parameter :: second_period_inverse=10.0,second_amplitude=0.0008,second_sin_shift=300.0 !2nd sinusoid, 2nd stage, parameters
	real,parameter :: normal_height=4.3,normal_distance=2.6,normal_width=5.205 !gaussian parameters
	real,parameter :: overload=5.0
	
	connections=size(weights(:,1,1,1))

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
					weights(to,from,column,row)=amplitude*sin(0.1*period*weights(to,from,column,row))+&
						gradient*weights(to,from,column,row)+height-&
						second_amplitude*sin((weights(to,from,column,row)+second_sin_shift)/second_period_inverse)+&
						normal_height*exp(-1.0*(((weights(to,from,column,row)-normal_distance)/normal_width)**2))
				!if weight is between 1050 and max_weight, just do a constant reduction
				else if ((weights(to,from,column,row)>1050.0) .and. (weights(to,from,column,row)<=max_weight)) then
					weights(to,from,column,row)=weights(to,from,column,row)-overload
				!limit weight to max_weight
				else
					weights(to,from,column,row)=max_weight
				end if
			end if
		end do
	end do

end subroutine weight_reducer
	
	
	
!this subroutine can be called to set weights, after the initialiser, but before the main loop, so as to direct neurons from the beginning	
subroutine preprogram(weights)
	
	real,dimension(*) :: weights(:,:,:,:)
	integer :: pathfinder, rows,columns
	
	rows=size(weights(1,1,1,:)); columns=size(weights(1,1,:,1))
	
	!here, I am directing data coming from the extreme left and extreme right to cross the network
	

	!subsequent nodes on the path from extreme left
	weights(8,2,1,1)=1000000.0
	do pathfinder=2,rows
		weights(8,1,pathfinder,pathfinder)=1000000.0
	end do	
	
	!subsequent nodes on the path from extreme right
	weights(6,2,columns,1)=1000000.0
	do pathfinder=2,rows
		weights(6,3,columns-pathfinder+1,pathfinder)=1000000.0
	end do	
	
end subroutine preprogram
	

	
end module welcome_to_dying