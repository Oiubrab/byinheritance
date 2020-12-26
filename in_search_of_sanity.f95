program in_search_of_sanity
use welcome_to_dying
use spiritechnology
!note, needs an angle input, from the command line, to run
!alternatively, place eudm in the folder and run it
implicit none

!network setup and reading
integer,parameter :: directions=8, rows=6, columns=11
integer :: blood_rows=rows+1
type(mind) :: think
logical :: file_exists

!sensing and response setup
integer, allocatable :: vision(:), response(:), response_counter(:,:)
integer :: movement, vision_place, new_vision_place, vision_centre, speed
integer :: vision_length=11, response_length=7
integer :: vision_socket=6, response_socket=6 !socket number represents where the middle of the corresponding array meets the brain network

!selecting and moving
integer :: row_number, column_number, row_number_2, column_number_2
integer :: column_number_3,info_number, row_random_number, column_random_number
integer,allocatable :: column_random(:),row_random(:)
integer :: moves, epoch, epoch_start

!from the outside
!note: cat angle left is 1st argument and cat angle right is 2nd argument
real,parameter :: pi=4.*asin(1./sqrt(2.))
real :: cat_angle_left,cat_angle_right,food
character(len=1000) :: angle_from_cat_left_cha,angle_from_cat_right_cha,food_cha,testing_cha
integer :: epoch_cutoff=100

!risk and reward
!note, grad must be less than (centre-1)/2
integer :: blood_rate=20
real :: blood_volume=8.0, blood_gradient=0.6, node_use_reward=2.0
real :: neuro_reward=1000.0,neuro_punishment=1000.0, straight_balance=1000.0, motivation_gradient=1.5

!testing
real :: random_see
logical :: testing=.false., show_blood=.true.
integer :: epoch_test_max=500, data_rate=20, random_probability=200
integer :: testrow,testcolumn,testorigin,testpoint,here

!timing
real :: start, finish, delay_time=0.0

!printing
character(len=:),allocatable :: column_cha


!test - caf image identifier
here=this_image()
if (here==1) then

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
	allocate(think%neurochem(2,columns,rows)) !allocate the reward variable, 1 is for origin, 2 is for point, 3 is for chemical_memory

	!find vision centre
	vision_centre=(size(vision)/2)+1


	!switch for turning on/off the test logs
	CALL GET_COMMAND_ARGUMENT(4,testing_cha)
	if (testing_cha=="true") then
		testing=.true.
	else if (testing_cha=="false") then
		testing=.false.
	ENDIF


	!read in the angle to the food
	CALL GET_COMMAND_ARGUMENT(1,angle_from_cat_left_cha)
	CALL GET_COMMAND_ARGUMENT(2,angle_from_cat_right_cha)	
	CALL GET_COMMAND_ARGUMENT(3,food_cha)	
	READ(angle_from_cat_left_cha,*)cat_angle_left
	READ(angle_from_cat_right_cha,*)cat_angle_right
	READ(food_cha,*)food

	!translate angle to all the foods into vision node
	call input_rules(vision,cat_angle_left,cat_angle_right)

	!if this is is a continuation of the algorithm, then load the previous cycle
	INQUIRE(FILE="will.txt", EXIST=file_exists)
	if (file_exists .eqv. .true.) then
		response=0
		!open the test log
		if (testing .eqv. .true.) then
			call read_write(think,epoch,moves,vision_place,"read",response_counter)
			epoch_start=epoch
			open(unit=1,file="test_log.txt",access="APPEND")
		else
			call read_write(think,epoch,moves,vision_place,"read")
			epoch_start=epoch
		end if
		
		
		
	!Otherwise, if this is the first time this network is activated, it has to be initialised
	else
		
		!initialise the network
		call initialiser(think,response,blood_volume,response_socket,response_counter)
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


		!print the first network for testing purposes
		if (testing .eqv. .true.) then
			!open the test log
			open(unit=1,file="test_log.txt")
			!save the network
			write(1,*)"By Inheritance"
			write(1,*)"Brain moves: 0 Epoch: 0"
			if (show_blood .eqv. .true.) then
				call print_network(vision,vision_socket,response,response_socket,think%brain_status,think%blood)
			else
				call print_network(vision,vision_socket,response,response_socket,think%brain_status)
			end if
		end if

	end if

	!!!reward!!!
	!currently rewards data moving towards middle of vision
	new_vision_place=findloc(vision,1,dim=1)
	call motivation(think%neurochem,think%brain_weight,vision_place,new_vision_place,&
		vision_centre,neuro_reward,neuro_punishment,straight_balance,motivation_gradient)



	!injection, from vision into brain
	do column_number=1,vision_length
		if (vision(column_number)==1) then	
			think%brain_status(1,plugin(column_number,vision_socket,vision_length,"brain"),1)=2
			think%brain_status(2,plugin(column_number,vision_socket,vision_length,"brain"),1)=1
		end if
	end do





	!This is the kernal, the grand daddy of this whole rotten affair
	call spiritech(epoch,think,blood_rate,response_socket,response_length,vision_length,vision_socket,blood_rows,epoch_cutoff,&
		blood_gradient,blood_volume,vision,response,response_counter,rows,columns,moves,testing,show_blood,delay_time,epoch_start)





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
			write(1,"(A28,I0,A1)")"Response counter for vision ",column_number,":"
			write(1,column_cha) response_counter(:,column_number)
		end do	

		!end timer
		call CPU_Time(finish)

		!print time elapsed
		write(1,*)" "
		call print_interval(start,finish)

		!finally, close the file
		close(1)

		!add response counter to will
		call read_write(think,epoch,moves,new_vision_place,"write",response_counter)

	else

		!place all the information network in a text file
		call read_write(think,epoch,moves,new_vision_place,"write")
	
	end if

	!temporary movement output
	movement=0
	speed=0
	do column_number_2=1,response_length
		if (response(column_number_2)==1) then
			
			!movement alteration
			if (column_number_2<=response_length-2) then
			
				!vision datum is moved x number of spots depending on the response datum's position off centre
				movement=-1*(((response_length-2)/2+1)-column_number_2) !neg is to the left, pos is to the right
				!halt the speed
				speed=0
			
			!speed alteration
			else
			
				!speed response is simple, left is neg and right is pos
				if (column_number_2==response_length-1) then
					speed=-1
				else if (column_number_2==response_length) then
					speed=1
				end if
				
				!halt the movement
				movement=0
				
			end if
			
			
		end if
	end do
	!print the shift
	print*,movement,speed


	!write an interpreter that takes a response array and outputs data in a format readable by the interface

end if

end program in_search_of_sanity
