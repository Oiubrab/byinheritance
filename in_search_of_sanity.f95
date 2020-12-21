program in_search_of_sanity
use welcome_to_dying
!note, needs an angle input, from the command line, to run
!alternatively, place eudm in the folder and run it
implicit none

!network setup and reading
integer,parameter :: directions=8, rows=6, columns=11
integer :: blood_rows=rows+1
type(mind) :: think
logical :: file_exists, proaction=.false.

!sensing and response setup
integer, allocatable :: vision(:), response(:), response_counter(:,:)
integer :: movement, vision_place, new_vision_place, vision_centre
integer :: vision_length=11, response_length=5
integer :: vision_socket=6, response_socket=6 !socket number represents where the middle of the corresponding array meets the brain network

!selecting and moving
integer :: row_number, column_number, row_number_2, column_number_2
integer :: column_number_3,info_number, row_random_number, column_random_number
integer,allocatable :: column_random(:),row_random(:)
integer :: moves, epoch, epoch_start

!from the outside
!note: cat angle left is 1st argument and cat angle right is 2nd argument
real,parameter :: pi=4.*asin(1./sqrt(2.))
real :: cat_angle_left,cat_angle_right
character(len=1000) :: angle_from_cat_left_cha,angle_from_cat_right_cha
integer :: epoch_cutoff=100

!risk and reward
!note, grad must be less than (centre-1)/2
integer :: blood_rate=20
real :: blood_volume=8.0, blood_gradient=0.6, node_use_reward=2.0
real :: neuro_reward=1000.0,neuro_punishment=1000.0, straight_balance=1000.0, motivation_gradient=1.5

!testing
real :: random_see
logical :: testing=.false., show_blood=.false.
integer :: epoch_test_max=5000, data_rate=20, random_probability=200
integer :: testrow,testcolumn,testorigin,testpoint

!timing
real :: start, finish, delay_time=0.1

!printing
character(len=:),allocatable :: column_cha



!start timer
call CPU_Time(start)

!fuck you
call random_seed()

!allocation block
allocate(character(columns*3+1) :: column_cha) !allocate the printing variable
allocate(vision(vision_length)) !allocate the array for input into the network, currently on top
allocate(response(response_length)) !allocate the array for output from the network, currently on bottom
allocate(response_counter(response_length,vision_length)) !allocat the array that will keep track of the response for printing purposes
allocate(column_random(columns)) !allocate the column selection randomiser (randomised at main loop start)
allocate(row_random(rows)) !allocate the row selection randomiser (randomised at main loop start)
allocate(think%brain_status(2,columns,rows)) !allocate the brain data and direction status, 1 is for direction, 2 is for data status
allocate(think%brain_weight(directions,directions,columns,rows)) !allocate the brain direction weighting 
allocate(think%blood(columns,blood_rows)) !allocate the gradient variable, extra row for response array
allocate(think%neurochem(2,columns,rows)) !allocate the reward variable, 1 is for origin, 2 is for point

!find vision centre
vision_centre=(size(vision)/2)+1

!if no command line argument is present, turn testing on
IF(COMMAND_ARGUMENT_COUNT().NE.2)THEN
	testing=.true.
ENDIF

!engage the precycle system setup
if (testing .eqv. .false.) then

	!read in the angle to the food
	CALL GET_COMMAND_ARGUMENT(1,angle_from_cat_left_cha)
	CALL GET_COMMAND_ARGUMENT(2,angle_from_cat_right_cha)	
	READ(angle_from_cat_left_cha,*)cat_angle_left
	READ(angle_from_cat_right_cha,*)cat_angle_right

	!translate angle to all the foods into vision node
	call input_rules(vision,cat_angle_left,cat_angle_right)

	!if this is is a continuation of the algorithm, then load the previous cycle
	INQUIRE(FILE="will.txt", EXIST=file_exists)
	if (file_exists .eqv. .true.) then
		call read_write(think,epoch,moves,vision_place,"read")
		epoch_start=epoch
	!Otherwise, if this is the first time this network is activated, it has to be initialised
	else

		!initialise the network
		call initialiser(think,response,blood_volume,response_socket)
		!call preprogram(think%brain_weight)

		do column_number=1,vision_length
			if (vision(column_number)==1) then	
				think%brain_status(1,plugin(column_number,vision_socket,vision_length,"brain"),1)=2
				think%brain_status(2,plugin(column_number,vision_socket,vision_length,"brain"),1)=1
			end if
		end do

		epoch=0
		epoch_start=0
		moves=0
		vision_place=vision_centre

	end if
	
	!!!reward!!!
	!currently rewards data moving towards middle of vision
	new_vision_place=findloc(vision,1,dim=1)
	call motivation(think%neurochem,think%brain_weight,vision_place,new_vision_place,&
		vision_centre,neuro_reward,neuro_punishment,straight_balance,motivation_gradient)

!if testing, a one time setup is needed
else

	!initialise the network
	call initialiser(think,response,blood_volume,response_socket)
	!call preprogram(think%brain_weight)
	!give vision a starting datum
	vision=0
	vision((vision_length/2)+1)=1
	!initialise counters
	epoch=0
	epoch_start=0
	moves=0
	
end if

!injection, from vision into brain
do column_number=1,vision_length
	if (vision(column_number)==1) then	
		think%brain_status(1,plugin(column_number,vision_socket,vision_length,"brain"),1)=2
		think%brain_status(2,plugin(column_number,vision_socket,vision_length,"brain"),1)=1
	end if
end do

!print the first network for testing purposes
if (testing .eqv. .true.) then
	print*,"By Inheritance"
	print*,"Brain moves: 0 Epoch: 0"
	if (show_blood .eqv. .true.) then
		call print_network(vision,vision_socket,response,response_socket,think%brain_status,think%blood)
	else
		call print_network(vision,vision_socket,response,response_socket,think%brain_status)
	end if
end if

!this is the new song
do while (proaction .eqv. .false.)

	!increment epoch
	epoch=epoch+1

	!add the blood gradient
	if (mod(epoch,blood_rate)==1) then
		do column_number=1,response_length
			think%blood(plugin(column_number,response_socket,response_length,"brain"),blood_rows)=&
				think%blood(plugin(column_number,response_socket,response_length,"brain"),blood_rows)+blood_volume	
		end do	
	end if
	do column_number=1,vision_length
		think%blood(plugin(column_number,vision_socket,vision_length,"brain"),1)=&
			think%blood(plugin(column_number,vision_socket,vision_length,"brain"),1)*0.7
	end do

	!first, randomise random row list
	call randomised_list(row_random)
	 
	!now I shall send randomly selected neurons to have data moved
	do row_number=1,rows

		!first, randomise column list for each row
		call randomised_list(column_random)
	
		do column_number=1,columns
		
			!now, assign the random integer positional number to the requisite random number positional number holder number
			column_random_number=column_random(column_number)
			row_random_number=row_random(row_number)
			
			!move the blood around
			call blood_mover(think%blood,column_random_number,row_random_number,blood_gradient)
			
			!only act on neurons that have data in them
			if (think%brain_status(2,column_random_number,row_random_number)==1) then

				!here is the important subroutine call that moves the data depending on how fat the neuron is 
				!individual data may move several times each loop. Loop these loops for a truly random movement (feature, not bug) 
				call selector(think,column_random_number,row_random_number,node_use_reward,response,response_socket,testing)		
							
				!lag if necessary
				if (testing .eqv. .true.) then
					call delay(delay_time)
				end if
				
				!increase the moves count
				moves=moves+1
			
				!print each step
				if (testing .eqv. .true.) then
					print*,"By Inheritance"
					print'(A15,I0,A8,I0)',"Brain moves: ",moves,"Epoch: ",epoch
					if (show_blood .eqv. .true.) then
						call print_network(vision,vision_socket,response,response_socket,think%brain_status,think%blood)
					else
						call print_network(vision,vision_socket,response,response_socket,think%brain_status)
					end if
					!keep track of response
					do column_number_2=1,response_length
						if (response(column_number_2)==1) then
							do column_number_3=1,vision_length
								if (vision(column_number_3)==1) then
									response_counter(column_number_2,column_number_3)=&
										response_counter(column_number_2,column_number_3)+1
								end if
							end do
						end if
					end do							
				end if	
					
				!print*,think%neurochem
				
				!response translation into movement
				do column_number_2=1,response_length
					if (response(column_number_2)==1) then
						
						!vision datum is moved x number of spots depending on the response datum's position off centre
						movement=-1*((response_length/2+1)-column_number_2) !neg is to the left, pos is to the right
						
						!stop the main loop
						if (testing .eqv. .false.) then
							proaction=.true.
							
						!in testing, move the vision datum as required
						else
							
							!get old vision position
							vision_place=findloc(vision,1,dim=1)
							!move vision according to movement
							vision=0
							
							!every so often move the vision datum randomly
							call random_number(random_see)
							if (int(random_see*float(random_probability))==(random_probability/2)) then
								vision(int(random_see)*vision_length+1)=1
								print*,"Random Shift"
							!otherwise, move the vision datum in response to the response datum position
							else if (vision_place+movement<1) then
								vision(vision_length+movement+vision_place)=1
							else if (vision_place+movement>vision_length) then
								vision(movement+(vision_length-vision_place))=1
							else
								vision(vision_place+movement)=1
							end if
							new_vision_place=findloc(vision,1,dim=1)
							response=0
							print*,"vision place, movement, new vision place:"
							print"(3(I3))",vision_place,movement,new_vision_place
							print*," "
							!activate motivation here
							call motivation(think%neurochem,think%brain_weight,vision_place,&
								new_vision_place,vision_centre,neuro_reward,neuro_punishment,straight_balance,motivation_gradient)
							
							!reset neurochem
							think%neurochem=0
							
							
						end if
						
					end if
				end do
				
			end if
			
			!the choosing weights reduce by one if they are not used and increase by node_use_reward-1 if they are used
			!this is done by subtracting one from all weights and adding node_use_reward to weights that are used
			call weight_reducer(think%brain_weight,column_random_number,row_random_number)
			
		end do

		!move the blood on the extra blood row
		if (row_number==rows) then
			call randomised_list(column_random)
			do column_number=1,columns
				!now, assign the random integer positional number to the requisite random number positional number holder number
				column_random_number=column_random(column_number)	
				!move the blood around
				call blood_mover(think%blood,column_random_number,blood_rows,blood_gradient)
			end do
		end if
		
	end do


	!if nothing has happened for epoch_cutoff, exit and output 0
	if (testing .eqv. .false.) then

		if ((epoch-epoch_start)>epoch_cutoff) then
			proaction=.true.
			movement=0
		end if
		
	!in testing conditions, continue untill maximum epoch reached
	!emit a vision datum after data_rate
	else 
	
		!emit a vision datum after data_rate
		if (mod(epoch-epoch_start,data_rate)==0) then
			!injection, from vision into brain
			do column_number=1,vision_length
				if (vision(column_number)==1) then	
					think%brain_status(1,plugin(column_number,vision_socket,vision_length,"brain"),1)=2
					think%brain_status(2,plugin(column_number,vision_socket,vision_length,"brain"),1)=1
				end if
			end do
			!reset epoch_start for next cycle
			epoch_start=epoch
		end if
		!end the mainloop
		if (epoch>=epoch_test_max) then
			proaction=.true.
		end if
	
	end if
	
end do

!print all the run data
if (testing .eqv. .true.) then

	!report results of each channel between vision and response at the end
	!first, setup printing format
	column_cha(1:1) = "("
	do column_number=1,response_length
		if (column_number==response_length) then
			column_cha(2+3*(column_number-1):2+3*(column_number-1)+2)="I4)"
		else
			column_cha(2+3*(column_number-1):2+3*(column_number-1)+2)="I4,"
		end if
	end do
	!now, print each combinating of vision and response
	do column_number=1,vision_length
		print"(A28,I0,A1)","Response counter for vision ",column_number,":"
		write(*,column_cha) response_counter(:,column_number)
	end do	

	!end timer
	call CPU_Time(finish)

	!print time elapsed
	print*," "
	call print_interval(start,finish)

else

	!place all the information network in a text file
	call read_write(think,epoch,moves,new_vision_place,"write")

	!print out the movement variable
	print"(I0)",movement

end if

end program in_search_of_sanity
