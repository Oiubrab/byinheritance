module welcome_to_dying

!define type for brain
type mind
	integer,allocatable :: brain_status(:,:,:)
	real,allocatable :: brain_weight(:,:,:)
	real,allocatable :: blood(:,:)
	real,allocatable :: neurochem(:,:)
end type mind

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

	integer,dimension(*) :: brain(:,:,:),vision(:),response(:)
	real,dimension(*) :: blood(:,:)
	integer :: row_counter,column_counter,rows,columns,info_ports,blood_rows,blood_columns
	!printing
	integer,parameter :: individual_width=2, individual_blood=5, separation_space=10
	character(len=individual_width) :: data_cha
	character(len=individual_blood) :: blood_cha
	character(len=12) :: individual_width_cha,separation_cha,individual_blood_cha
	character(len=17) :: width,blood_width
	character(len=:),allocatable :: print_row
	
	!establish network dimensions
	rows=size(brain(1,1,:)); columns=size(brain(1,:,1)); info_ports=size(brain(:,1,1))
	blood_rows=size(blood(1,:)); blood_columns=size(blood(:,1))
	
	!allocate the printing row with enough space to fit both brain and blood
	allocate(character((columns*individual_width)+(blood_columns*individual_blood)+2+separation_space) :: print_row) !allocate the printing variable
	
	!set the width to print for each datum
	write(individual_width_cha,*)individual_width
	write(individual_blood_cha,*)individual_blood
	!width for brain/blood
	width="(I"//trim(individual_width_cha)//")"
	blood_width="(F"//trim(individual_blood_cha)//".2)"

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
		do column_counter=1,columns+blood_columns+1
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
				write(blood_cha,blood_width)blood(column_counter-(columns+1),row_counter)
				!from: brain columns + separation + blood column number * blood column width - (blood column width + 1)
				!to: brain columns + separation + blood column number * blood column width
				print_row(columns*individual_width+separation_space+(column_counter-(columns+1))*&
					individual_blood-(individual_blood-1):columns*individual_width+separation_space+&
					(column_counter-(columns+1))*individual_blood)=blood_cha
			end if
		end do
		print *,print_row
	end do
	print*," "

	!print the response and equivalent blood arrays last
	do column_counter=1,columns+blood_columns+1
		!add response numbers to the first half of the row
		if (column_counter<=columns) then
			write(data_cha,width)response(column_counter)
			print_row(column_counter*individual_width-(individual_width-1):column_counter*individual_width)=data_cha
			!create a break for the two networks
		else if (column_counter==columns+1) then
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




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!planting and payoff - initialisation and data moving!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!this function selects the neuron to be targeted and sends data from the current neuron (in column,row) to the targeted neuron
!it also currently handles the increase in weights that correspond to data moving through a specific route 
subroutine selector(idea,column,row,reward,response,printer)

	type(mind) :: idea
	integer,allocatable :: response(:)
	real,allocatable :: rungs(:),brain_select(:)
	real :: increment, fuck, reward, blood_trans=0.05
	integer,intent(in) :: row,column
	integer :: point, connections, data_pos, origin, tester, columnmax, rowmax, counter, second_point
	character(len=*) :: printer
	
	!brain size
	rowmax=size(idea%brain_status(1,1,:)); columnmax=size(idea%brain_status(1,:,1))
	
	!setup amount of data ports, data position and origin label
	connections=int(sqrt(float(size(idea%brain_weight(:,1,1))))) !8 for adjacent setup
	data_pos=size(idea%brain_status(:,1,1))
	origin=idea%brain_status(data_pos-1,column,row)
	
	!brain_select collects all the weights, as directed in the point variable contained in the second last brain entry (data_pos-1)
	allocate(brain_select(connections))
	do point=1,size(brain_select)
		if ((point_to_neuron(column,row,point,"column")>0) .and. &
			(point_to_neuron(column,row,point,"row")>0) .and. &
			(point_to_neuron(column,row,point,"column")<columnmax+1) .and. &
			(point_to_neuron(column,row,point,"row")<rowmax+1)) then
			
				if (idea%brain_status(data_pos,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))==1) then
					!label destinations that have data already in them with 0 weight
					brain_select(point)=0.

				else
					!select the set of weights that correspond to the eight directions, as directed by the data's origin (data_pos-1)
					!scale by blood in the pointed channel
					brain_select(point)=idea%brain_weight(((origin-1)*connections)+point,column,row)*&
						idea%blood(point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))

				end if
				
		else

			!just hard coding in bottom option because it's too late in the night to fix. need to generalise for multi dimensional output
			if ((point_to_neuron(column,row,point,"row")==rowmax+1) .and. &
				(point_to_neuron(column,row,point,"column")>0) .and. &
				(point_to_neuron(column,row,point,"column")<columnmax+1)) then
				
				brain_select(point)=idea%brain_weight(((origin-1)*connections)+point,column,row)*&
					idea%blood(point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))
			
			else
				!select the set of weights that correspond to the eight directions, as directed by the data's origin (data_pos-1)
				brain_select(point)=idea%brain_weight(((origin-1)*connections)+point,column,row)
			end if

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
	if (brain_select(1)==0.0) then
		rungs(1)=0.0
	else
		rungs(1)=increment+brain_select(1)
	end if
	!set the subsequent rungs
	do point=2,size(brain_select)
		if (brain_select(point)==0.0) then
			rungs(point)=rungs(point-1)
		else
			rungs(point)=rungs(point-1)+increment+brain_select(point)
		end if
	end do

	if (printer=="yes") then
		print*,"Maximum Rungs Value, choice value:"
		print*,rungs(size(rungs)),fuck*rungs(size(rungs))
		print*,"Weightings from Direction 1 to 8:"
		print"(F7.2,F7.2,F7.2,F7.2,F7.2,F7.2,F7.2,F7.2)",brain_select
	end if

	!if there is nowhere for the data to go, it has to stay here. Otherwise, find a new home 
	if (rungs(size(rungs))/=0.0) then
			
		!scale the fuck to be the same range as the rungs set
		fuck=fuck*rungs(size(rungs))
		
		!take the chosen neuron and move the data to it
		do point=1,size(brain_select)
			if (fuck<=rungs(point)) then
				
				!moving diagnostic
				if (printer=="yes") then
					print*,"Move from:"
					print*,column,row
					print*,"Move to:"
					print*,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row")
					print*," "
				end if
			
				!add to weight selection. Weight add should overcome global reduction
				idea%brain_weight(((origin-1)*connections)+point,column,row)=idea%brain_weight(((origin-1)*connections)+point,column,row)+reward
				!remove data and position indicator from current neuron
				idea%brain_status(data_pos,column,row)=0
				idea%brain_status(data_pos-1,column,row)=0
				
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
subroutine blood_mover(blood,column_num,row_num)

	real,dimension(*) :: blood(:,:)
	integer,allocatable :: row_randomised(:), column_randomised(:)
	integer :: column_num,row_num,row_select,column_select
	real :: hope, fear, dist, distil, transition
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
			transition=exp(-(dist*(sigmoid(blood(column_number_2,row_number_2),"forward",1.0,1.0,0.0,0.0)**(-1.)))**2)	
			
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
!the openside variable decides which side data will flow out from
subroutine initialiser(thought,vision,response,openside)

	integer,dimension(*) :: vision(:), response(:)
	integer :: row_number, column_number, info_number
	integer :: rows, columns, info_ports
	integer :: modding=8,resultant
	character(len=*) :: openside
	type(mind) :: thought
	
	rows=size(thought%brain_weight(1,1,:)); columns=size(thought%brain_weight(1,:,1)) 
	info_ports=size(thought%brain_status(:,1,1)); info_weight=size(thought%brain_weight(:,1,1))
	do row_number=1,rows
		do column_number=1,columns
		
			!zero out brain_status
			thought%brain_status(1,column_number,row_number)=0
			thought%brain_status(2,column_number,row_number)=0			
			
			do info_number=1,info_weight
			
				!set up the ones and zeroes
				!zero for any weight directing data off the top if the top isn't open
				if ((point_to_neuron(column_number,row_number,weight_direction(info_number),"row")==0) .and. &
					(openside/="top")) then
					
					thought%brain_weight(info_number,column_number,row_number)=0.	
								
				!or bottom
				else if ((point_to_neuron(column_number,row_number,weight_direction(info_number),"row")==rows+1) .and. &
					(openside/="bottom")) then

					thought%brain_weight(info_number,column_number,row_number)=0.
					
				!zero for any weight diracting data off the left if it isn't open
				else if ((point_to_neuron(column_number,row_number,weight_direction(info_number),"column")==0) .and. &
					(openside/="left")) then
					
					thought%brain_weight(info_number,column_number,row_number)=0.
					
				!or right
				else if ((point_to_neuron(column_number,row_number,weight_direction(info_number),"column")==columns+1) .and. &
					(openside/="right")) then
					
					thought%brain_weight(info_number,column_number,row_number)=0.
					
				!otherwise, all weights start off at one
				else
					thought%brain_weight(info_number,column_number,row_number)=1.
				end if
				
			end do
		end do
	end do
	
	!setting up blood network, below only adds 0.1 to every entry
	do row_number=1,rows+1
		do column_number=1,columns
			
			thought%blood(column_number,row_number)=0.01
				
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
	
	
	

!!!!!!!!!!!!!!!!!!!
!!weight handling!!
!!!!!!!!!!!!!!!!!!!



subroutine weight_reducer(weights,column,row)

	real,dimension(*) :: weights(:,:,:)
	integer :: info_number,info_ports,column,row
	real,parameter :: max_height=193.0, start_axis=6.8, stretch=24.6, bottom=-5.2 !sigmoid parameters
	real,parameter :: log_stretch=3.5,log_move=1.1,log_shift=1.1 !natural log parameters
	real,parameter :: normal_height=4.3,normal_shift=-0.9,normal_stretch=13.0 !gaussian parameters
	real :: height=max_height-bottom
	
	info_ports=size(weights(:,1,1))

	do info_number=1, info_ports
		if (weights(info_number,column,row)>1.) then
			!weight is scaled down by a sigmoid (significant over large numbers) plus a natural log (significant over medium numbers) 
			!plus a gaussian (significant over small numbers)
			weights(info_number,column,row)=sigmoid(weights(info_number,column,row),"forward",height,stretch,bottom,start_axis)+&
				log_stretch*log(log_move*weights(info_number,column,row)+log_shift)+&
				normal_height*exp((normal_shift-weights(info_number,column,row))/normal_stretch)
		end if
	end do

end subroutine weight_reducer
	
end module welcome_to_dying