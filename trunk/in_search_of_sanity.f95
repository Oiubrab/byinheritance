program in_search_of_sanity
use welcome_to_dying
implicit none

!network setup
integer,parameter :: info_ports=64, rows=5, columns=7
integer :: blood_rows=rows+1
type(mind) :: think

!sensing and response setup
integer, allocatable :: vision(:), response(:), response_counter(:,:) !vision is rows, response is columns
integer,parameter :: data_rate=20
real :: look !test variable for randomisiong vision array
character(len=6) :: opener="bottom"

!selecting and moving
integer :: row_number, column_number, row_number_2, column_number_2
integer :: column_number_3,info_number, row_random_number, column_random_number
integer,allocatable :: column_random(:),row_random(:)
integer :: moves=0, epoch, epoch_total=5000

!risk and reward
integer :: blood_rate=15
real :: blood_volume=5.0, blood_gradient=0.8, use_reward=200.0

!timing
real :: start, finish, delay_time=0.2

!printing
character(len=3) :: print_yesno="yes"
character(len=:),allocatable :: column_cha



!start timer
call CPU_Time(start)

!allocation block
allocate(character(columns*3+1) :: column_cha) !allocate the printing variable
allocate(vision(columns)) !allocate the array for input into the network, currently on top
allocate(response(columns)) !allocate the array for output from the network, currently on bottom
allocate(response_counter(columns,columns)) !allocat the array that will keep track of the response for printing purposes
allocate(column_random(columns)) !allocate the column selection randomiser (randomised at main loop start)
allocate(row_random(rows)) !allocate the row selection randomiser (randomised at main loop start)
allocate(think%brain_status(2,columns,rows)) !allocate the brain data and direction status, 1 is for direction, 2 is for data status
allocate(think%brain_weight(info_ports,columns,rows)) !allocate the brain direction weighting 
allocate(think%blood(columns,blood_rows)) !allocate the gradient variable, extra row for response array
allocate(think%neurochem(columns,rows)) !allocate the reward variable

!initialise the network
call initialiser(think,vision,response,opener)

!test injection - an origin must accompany the data
vision(2)=1 !change this to detect the food position when I attach this to the game
do column_number=1,columns
	if (vision(column_number)==1) then	
		think%brain_status(1,column_number,1)=2
		think%brain_status(2,column_number,1)=1
	end if
end do

if (print_yesno=="yes") then
	print*,"By Inheritance"
	print*,"Brain moves: 0 Epoch: 0"
	call print_network(think%brain_status,think%blood,vision,response)
end if

!this is the new song
do epoch=1,epoch_total

	!add the blood gradient
	if (mod(epoch,blood_rate)==1) then
		do column_number=1,columns 
			think%blood(column_number,blood_rows)=think%blood(column_number,blood_rows)+blood_volume	
		end do	
	end if
	do column_number=1,columns
		think%blood(column_number,1)=think%blood(column_number,1)*0.7
	end do

	!injection from vision into brain
	if (mod(epoch,data_rate)==1) then
		do column_number=1,columns
			if (vision(column_number)==1) then	
				think%brain_status(1,column_number,1)=2
				think%brain_status(2,column_number,1)=1
				
			end if
		end do
	end if

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
				call selector(think,column_random_number,row_random_number,use_reward,response,print_yesno)
							
				!lag if necessary
				call delay(delay_time)
				
				!increase the moves count
				moves=moves+1
			
				!print each step
				if (print_yesno=="yes") then
					print*,"By Inheritance"
					print'(A15,I0,A8,I0)',"Brain moves: ",moves,"Epoch: ",epoch
					call print_network(think%brain_status,think%blood,vision,response)
					!keep track of response
					do column_number_2=1,columns
						if (response(column_number_2)==1) then
							do column_number_3=1,columns
								if (vision(column_number_3)==1) then
									response_counter(column_number_2,column_number_3)=&
										response_counter(column_number_2,column_number_3)+1
								end if
							end do
						end if
					end do							
				end if
				
				!test - response moves vision
				do column_number_2=1,columns
					if (response(column_number_2)==1) then
						vision=0
						vision(columns-column_number_2+1)=1
						response=0
					end if
				end do
				!call random_number(look)
				
			end if
			
			!the choosing weights reduce by one if they are not used and increase by use_reward-1 if they are used
			!this is done by subtracting one from all weights and adding use_reward to weights that are used
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

end do

!report results of each channel between vision and response at the end
!first, setup printing format
column_cha(1:1) = "("
do column_number=1,columns
	if (column_number==columns) then
		column_cha(2+3*(column_number-1):2+3*(column_number-1)+2)="I4)"
	else
		column_cha(2+3*(column_number-1):2+3*(column_number-1)+2)="I4,"
	end if
end do
!now, print each combinating of vision and response
do column_number=1,columns
	print"(A28,I0,A1)","Response counter for vision ",column_number,":"
	write(*,column_cha) response_counter(:,column_number)
end do	

!end timer
call CPU_Time(finish)

!print time elapsed
print*," "
call print_interval(start,finish)

end program in_search_of_sanity
