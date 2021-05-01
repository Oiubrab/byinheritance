module welcome_to_dying

!define type for brain
type mind
	integer,allocatable :: brain_status(:,:,:) !allocate the brain data and direction status, 1 is for the origin direction of data, 2 is for data status
	real,allocatable :: brain_weight(:,:,:,:) !allocate the brain direction weighting. 8,:,:,: holds the weights from origin :,x,:,: to point x,:,:,:
	real,allocatable :: blood(:,:) !allocate the gradient variable, extra row for response array
	integer,allocatable :: neurochem(:,:,:,:) !allocate the reward variable, 1,:,:,: is for origin, 2,:,:,: is for point. :,10,:,: is the weight ladder
end type mind

!define input output type
type see_say
	integer,allocatable :: vision(:),response(:)
end type see_say	

contains

!this is where the magic happens



















!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!housekeeping - time and metadata!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!








!returns a list of sequential numbers up to a length defined by the array input, in a list where the order of the numbers has been randomised
!master_killer is the allocatable array argument
pure subroutine randomised_list(master_killer,ran_dom)
 	!$acc routine seq
	integer,dimension(*),intent(inout) :: master_killer(:)
	integer :: roger_explosion, length, pos_hold
	integer,intent(inout) :: ran_dom
	real :: despair
	!$acc routine(random_something) seq

		length=size(master_killer)

		!make sequential array of length defined by the array size
		do roger_explosion=1,length
			master_killer(roger_explosion)=roger_explosion
		end do
	
		!swap elements until the list is randomized
		do roger_explosion=1,length
			call random_something(despair,ran_dom)
			pos_hold=master_killer(roger_explosion)
			master_killer(roger_explosion)=master_killer(int(length*despair)+1)
			master_killer(int(length*despair)+1)=pos_hold
		end do

end subroutine randomised_list



!read in the network from a text file or write out to a text file
subroutine read_write(imagine,imagination,think,direction)
	type(mind) :: think
	integer :: epoch,imagine,imagination
	character(len=20) :: willfull
	character(len=*) :: direction
	integer :: column,row
	
	write(willfull,"(A4,I0,A4)") "will",imagine,".txt"
	!print*,imagine,willfull,"read_write"
	
	if (direction=="read") then
	
		!retrieve previous network
		open(unit=imagination+imagine,file=willfull)
		read(imagination+imagine,*) think%brain_status
		read(imagination+imagine,*) think%brain_weight
		read(imagination+imagine,*) think%blood
		read(imagination+imagine,*) think%neurochem				
		close(imagination+imagine)
		
	else if (direction=="write") then
	
		!write the networks to file
		open(unit=2*imagination+imagine,file=willfull)
		write(2*imagination+imagine,*) think%brain_status
		write(2*imagination+imagine,*) think%brain_weight
		write(2*imagination+imagine,*) think%blood
		write(2*imagination+imagine,*) think%neurochem		
		close(2*imagination+imagine)
	
	end if
	
end subroutine read_write






!this subroutine uses a seed to start a random number generation pattern
!this subroutine is a replacement for fortran's random number generator, that does not
!work with concurrent loops
!why is the number to be randomised and seeder is the seed number (passed out for the next call)
pure subroutine random_something(why,eye)
 	!$acc routine seq
	real,parameter :: pi=asin(1.0)*2.0,stretch=1000.0,knockout=10000.0
	integer,parameter :: width=100,algo_num=3,periods=10,turns=1000
	real,intent(inout) :: why
	real :: ex,de_ex_1,de_ex_4
	integer,intent(inout) :: eye
	integer :: see,turn_count

	!establish the domain
	ex=float(eye)/float(width)
	!Rather than continue along the chain, start subdividing the domain
	if (ex>float(periods)) then
		turn_count=0
		do while (ex>float(periods))
			ex=ex-float(periods)
			turn_count=turn_count+1
		end do
		ex=ex+((float(eye)+float(turn_count))/(float(width)*float(turns)))
	end if
	!simple sinusoid
	if (mod(eye,algo_num)==0) then
		!apply the function
		why=abs(sin(exp(ex)))
	!superimposed sinusoids x and 2x, normalised to range (0,1)
	else if (mod(eye,algo_num)==1) then
		!the derivitive domain zero (first n)
		de_ex_1=2.0*pi-2.0*atan(sqrt(6.0-sqrt(33.0)))
		!the derivitive domain zero (fourth n)
		de_ex_4=8.0*pi-2.0*atan(sqrt(6.0-sqrt(33.0)))
		!apply the function
		why=(sin(ex)+sin(2.0*ex)-(sin(de_ex_4)+sin(2.0*de_ex_4)))/(-2.0*(sin(de_ex_1)+sin(2.0*de_ex_1)))
	!superimposed sinusoids pi*x and 3x, normalised to range (0,1)
	else if (mod(eye,algo_num)==2) then
		!the derivitive domain zero (first n) (approximate)
		de_ex_1=0.511255495232408
		!apply the function
		why=(sin(pi*ex)+sin(3.0*ex)+sin(pi*de_ex_1)+sin(3.0*de_ex_1))/(2.0*(sin(pi*de_ex_1)+sin(3.0*de_ex_1)))
	end if
	
	!grow why, negate, then return to original range through division
	why=((stretch*why)-why)/stretch
	!grow why, floor why, then return to original size and subtract from original why, killing width*stretch orders of magnitude. Re-expand as width*stretch for original range
	why=(why-float(int(why*(knockout)))/(knockout))*(knockout) 
	if (why==0.0000000000) why=0.000000001
	if ((why>1.0) .or. (why<0.0)) why=abs(why)-float(int(abs(why)))
	if (why==1.0) why=0.99999 
	eye=eye+1
	

end subroutine

















!!!!!!!!!
!!maths!!
!!!!!!!!!


!this function either applies a sigmoid (forward) or inverse sigmoid (reverse) function depending on flow variable
!note, inverse sigmoid only goes up to 16*range_stretch
pure function sigmoid(insig,flow,range_stretch,domain_stretch,range_shift,domain_shift) result(outsig)
 	!$acc routine seq	
	real,intent(in) :: insig
	real,intent(in) :: range_stretch,domain_stretch,range_shift,domain_shift
	real :: outsig
	character(len=*),intent(in) :: flow

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





!!!!!!!!!!!!!!!!
!!!conversion!!!
!!!!!!!!!!!!!!!!


!this function takes a positional binary array that represents a percentage (-100,100) and returns an approximation of that percentage
pure function position_to_percentage(positional) result(percent)
 	!$acc routine seq
	integer,dimension(*),intent(in) :: positional(:)
	integer :: percent,pos
	real :: data_width
	
	!find the width between each binary position
	!note, total width is (-100,100)
	data_width=200.0/float(size(positional))
	
	!find the data position and convert into percentage approximation
	do pos=1,size(positional)
		if (positional(pos)==1) then
			percent=-100+int((float(pos-1)*data_width)+(((float(pos)*data_width)-(float(pos-1)*data_width))/2.0))
		end if
	end do
			
end function




!this function takes a binary array and outputs an integer
!the last bit represents pos (1) or neg (0)
!note; the binary array must be inputted in format left:smallest to right:highest
pure function binary_to_decimal(binary_array) result(decimal)
 	!$acc routine seq
	integer,dimension(*),intent(in) :: binary_array(:)
	integer :: order, decimal, binary_size
	
	!size the array and zero the integer
	binary_size=size(binary_array)
	decimal=0
	
	!conversion algorithm: if binary position is 1, add 2^order to the integer untill all the positions (except last) have been taken
	do order=0, binary_size-2
		decimal = decimal + binary_array(order+1)*(2**order)
		!print*,decimal,binary_array(order+1),((binary_array(order+1))*2)
	end do
	
	!make negative integer if the negative trigger is set
	if (binary_array(binary_size)==0) then
		decimal=decimal*(-1)
	end if
	
end function
		





















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
pure function point_to_neuron(column_in,row_in,point,rowcolumn) result(finder)
 	!$acc routine seq
	integer,intent(in) :: column_in, row_in, point
	character(len=*),intent(in) :: rowcolumn
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
pure function point_origin(point) result(origin)
 	!$acc routine seq
	integer,intent(in) :: point
	integer :: origin
	
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
pure function weight_direction(weighting) result(resultant)
 	!$acc routine seq
	integer,parameter :: modding=8
	integer,intent(in) :: weighting
	integer :: resultant

	resultant=mod(weighting,modding)
	if (resultant==0) then
		resultant=modding
	end if

end function weight_direction



!this function calculates the position of the corresponding brain/array column, given an array/brain column and a socket
!socket number represents where the middle of the corresponding array meets the brain network
!label is what is meant to be found, i.e brain label will give the brain column for a given response array position
pure function plugin(in_pos,socket,com_length,label) result(out_pos)
 	!$acc routine seq
	character(len=*),intent(in) :: label
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







!this function selects the neuron to be targeted and sends data from the current neuron (in column,row) to the targeted neuron
!it also currently handles the increase in weights that correspond to data moving through a specific route 
pure subroutine selector(idea,column,row,reward,response,response_socket,ran_dom)
 	!$acc routine seq
	type(mind),intent(inout) :: idea
	integer,allocatable :: connection_translation(:)
	integer,dimension(*),intent(inout) :: response(:)
	real,allocatable :: rungs(:)
	real :: increment, fuck
	real,intent(in) :: reward
	real,parameter :: blood_trans=0.05
	integer,intent(in) :: row,column,response_socket
	integer,intent(inout) :: ran_dom
	integer :: rung, point, connections, data_pos, origin, rank, rank_size
	integer :: columnmax, rowmax, counter, second_point, response_length
	character(len=20) :: tester
	
	!accelerator subroutine and function declarations		
	!$acc routine(point_to_neuron) seq	
	!$acc routine(random_something) seq	
	!$acc routine(randomised_list) seq
	!$acc routine(plugin) seq	
	
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

	call random_something(fuck,ran_dom)

	!setup connection_translation, a randomising position for the rungs - weights selector
	allocate(connection_translation(connections))
	call randomised_list(connection_translation,ran_dom)

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
			!if data is in the pointed-to node, zero the probability
			if (idea%brain_status(data_pos,point_to_neuron(column,row,point,"column"),point_to_neuron(column,row,point,"row"))==1) then
				if (rung==1) then
					rungs(rung)=0.0
				else
					rungs(rung)=rungs(rung-1)
				end if		
			!otherwise, for an open node in network, add the weight and the blood modifier
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


	!if there is nowhere for the data to go, it has to stay here. Otherwise, find a new home 
	if (rungs(size(rungs))/=0.0) then
			
		!scale the fuck to be the same range as the rungs set
		fuck=fuck*rungs(size(rungs))
		
		!take the chosen neuron and move the data to it
		do rung=1,connections
			if (fuck<=rungs(rung)) then
				
				!translate rung into corresponding weight
				point=connection_translation(rung)
			
				!add to weight selection. Weight add should overcome global reduction
				idea%brain_weight(point,origin,column,row)=idea%brain_weight(point,origin,column,row)+reward
				!remove data and position indicator from current neuron
				idea%brain_status(data_pos,column,row)=0
				idea%brain_status(data_pos-1,column,row)=0
				
				!add data to neurochem
				!first, move each rank down one rung and push the last rank off the ladder
				rank_size=size(idea%neurochem(1,:,1,1))
				do rank=1,rank_size-1
					idea%neurochem(1,rank_size-rank+1,column,row)=idea%neurochem(1,rank_size-rank,column,row)
					idea%neurochem(2,rank_size-rank+1,column,row)=idea%neurochem(2,rank_size-rank,column,row)									
				end do
				!now, move the current activation to the top rung
				idea%neurochem(1,1,column,row)=origin
				idea%neurochem(2,1,column,row)=point				
				
				!if data is moving off the brain, direct it into the response array	
				!can add more response options here if I want to add more response directions
				!for instance, something off of the left or right sides of the network
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
pure subroutine blood_mover(blood,column_num,row_num,gradient,ran_dom)
 	!$acc routine seq
	real,dimension(*),intent(inout) :: blood(:,:)
	integer,allocatable :: row_randomised(:), column_randomised(:)
	integer,intent(in) :: column_num,row_num
	integer :: row_select,column_select
	integer,intent(inout) :: ran_dom
	real :: hope, fear, dist, distil, transition
	real,intent(in) :: gradient !controls how quickly blood flows. bigger is faster
	integer :: rows,columns,row_number_2,column_number_2

	!accelerator subroutine and function declarations		
	!$acc routine(randomised_list) seq
	!$acc routine(random_something) seq	
	!$acc routine(sigmoid) seq
	
	rows=size(blood(1,:)); columns=size(blood(:,1))

	!make the random arrays as big as columns and rows
	allocate(row_randomised(rows))
	allocate(column_randomised(columns))
	
	!randomise the row array
	call randomised_list(row_randomised,ran_dom)

	!engage every blood neuron around the neuron in question
	do row_select=1,rows
		!first, randomise the row selection
		row_number_2=row_randomised(row_select)
		!before each row pass, randomise the column selection
		call randomised_list(column_randomised,ran_dom)
		
		do column_select=1,columns
			!randomise the column selection
			column_number_2=column_randomised(column_select)

			!set the prospective transitionn value random multipliers
			call random_something(hope,ran_dom)
			call random_something(fear,ran_dom)
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
pure subroutine initialiser(thought,volume,response_socket,response_columns)
 	!$acc routine seq
	integer :: row_number, column_number, path_from, path_to, paths
	integer :: rows, columns, info_ports,blood_rows
	integer,intent(in) :: response_socket, response_columns
	real,intent(in) :: volume
	type(mind),intent(inout) :: thought

	!accelerator subroutine and function declarations		
	!$acc routine(point_to_neuron) seq	
	!$acc routine(plugin) seq		
	
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
	

end subroutine initialiser
	
	








	

!!!!!!!!!!!!!!!!!!!
!!weight handling!!
!!!!!!!!!!!!!!!!!!!





!This subroutine takes the neurochem matricies saved in each network and the output of the motivate network and applies a scaling factor to the weights of each network
!first argument must be the network and the second argument must be the multiplier from the motive network (oddsey)
pure subroutine animus(meaning,oddyseus)
 	!$acc routine seq
	type(mind),intent(inout) :: meaning
	integer,intent(in) :: oddyseus 
	integer :: row, column, to, from, ladder

	!add to weights based on the neurochem at that node and the scaling 
	do row=1,size(meaning%brain_weight(1,1,1,:))
		do column=1,size(meaning%brain_weight(1,1,:,1))
			do ladder=1,size(meaning%neurochem(1,:,1,1))
			
				!for each rung in the neurochem ladder, establish which weight is to be altered
				from=meaning%neurochem(1,ladder,column,row)
				to=meaning%neurochem(2,ladder,column,row)
				
				!exclude ladder rungs that haven't been set yet
				if ((to/=0) .and. (from/=0)) then 
					!exclude intentionally zeroed out weights
					if (meaning%brain_weight(to,from,column,row)/=0.0) then
					
						!add the weight
						!the lower the weight is on the ladder, the smaller the modulation
						meaning%brain_weight(to,from,column,row)=meaning%brain_weight(to,from,column,row)+&
							(size(meaning%neurochem(1,:,1,1))+1-ladder)*oddyseus
							
						!if weight manipulation reduces weight below 1.0, make it 1.0
						if (meaning%brain_weight(to,from,column,row)<1.0) then
							meaning%brain_weight(to,from,column,row)=1.0
						end if
						 
					end if
				end if	
				
			end do
		end do
	end do

end subroutine






!This subroutine controls the weight reductions per weight per action
pure subroutine weight_reducer(weights,column,row)
 	!$acc routine seq
	real,dimension(*),intent(inout) :: weights(:,:,:,:)
	integer :: from,to,connections
	integer,intent(in) :: column,row
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
				!if weight is between 1050 and 1000000, just do a constant reduction
				else if ((weights(to,from,column,row)>1050.0) .and. (weights(to,from,column,row)<=1000000.0)) then
					weights(to,from,column,row)=weights(to,from,column,row)-overload
				!limit weight to 1000000
				else
					weights(to,from,column,row)=1000000.0
				end if
			end if
		end do
	end do

end subroutine weight_reducer

	

	
end module welcome_to_dying
