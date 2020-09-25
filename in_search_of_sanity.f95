program in_search_of_sanity
use welcome_to_dying
implicit none

!define type for brain
type mind
	integer,allocatable :: brain_status(2,:,:)
	real,allocatable :: brain_weight(64,:,:)
end type mind

!network setup
integer,parameter :: info_ports=66 !first 64 are weights, 65 the origin address of data (if present), 66 is data port
integer,parameter :: blood_ports=2 !1 is neurochem, 2 is blood
integer :: rows=6, columns=11
integer, allocatable :: brain(:,:,:), brain_weights
real,allocatable :: blood(:,:,:)

!sensing and response setup
integer, allocatable :: vision(:), response(:)
integer,parameter :: data_rate=50
real :: look !test variable for randomisiong vision array
character(len=6) :: opener="bottom"

!selecting and moving
integer :: row_number, column_number, row_number_2, column_number_2, info_number, row_random_number, column_random_number
integer,allocatable :: column_random(:),row_random(:)
integer :: moves=0, epoch, epoch_total=500

!risk and reward
integer :: use_reward=20 ,blood_rate=20
real :: blood_gradient=1.0

!timing
real :: start, finish, delay_time=0.1

!printing
character(len=3) :: print_yesno="yes"



!start timer
call CPU_Time(start)

!allocation block
allocate(vision(columns)) !allocate the array for input into the network, currently on top
allocate(response(columns)) !allocate the array for output from the network, currently on bottom
allocate(brain(info_ports,columns,rows)) !allocate the network variable
allocate(blood(blood_ports,columns,rows)) !allocate the reward variable
allocate(column_random(columns)) !allocate the column selection randomiser (randomised at main loop start)
allocate(row_random(rows)) !allocate the row selection randomiser (randomised at main loop start)


!initialise the network
call initialiser(brain,blood,vision,response,opener)

!test injection - an origin must accompany the data
vision(2)=1 !change this to detect the food position when I attach this to the game
do column_number=1,columns
	if (vision(column_number)==1) then	
		brain(info_ports-1,column_number,1)=2
		brain(info_ports,column_number,1)=1
	end if
end do

if (print_yesno=="yes") then
	print*,"Brain moves: 0 Epoch: 0"
	call print_network(brain,blood,vision,response)
end if

!this is the new song
do epoch=1,epoch_total

	!add the blood gradient
	if (mod(epoch,blood_rate)==0) then
		do column_number=1,columns 
			blood(blood_ports,column_number,rows)=blood(blood_ports,column_number,rows)+blood_gradient		
		end do	
	end if
	do column_number=1,columns
		blood(blood_ports,column_number,1)=blood(blood_ports,column_number,1)*0.5
	end do

	!injection from vision into brain
	if (mod(epoch,data_rate)==0) then
		do column_number=1,columns
			if (vision(column_number)==1) then	
				brain(info_ports-1,column_number,1)=2 !an origin must accompany the data
				brain(info_ports,column_number,1)=1
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
			call blood_mover(blood,column_random_number,row_random_number)
			
			!only act on neurons that have data in them
			if (brain(info_ports,column_random_number,row_random_number)==1) then
			
				!here is the important subroutine call that moves the data depending on how fat the neuron is 
				!individual data may move several times each loop. Loop these loops for a truly random movement (feature, not bug) 
				call selector(blood,brain,column_random_number,row_random_number,use_reward,response,print_yesno)
							
				!lag if necessary
				call delay(delay_time)
				
				!increase the moves count
				moves=moves+1

				
				!print each step
				if (print_yesno=="yes") then
					print'(A15,I0,A8,I0)',"Brain moves: ",moves,"Epoch: ",epoch
					call print_network(brain,blood,vision,response)
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
			do info_number=1, info_ports-2
				if (brain(info_number,column_random_number,row_random_number)>1) then
					brain(info_number,column_random_number,row_random_number)=&
						brain(info_number,column_random_number,row_random_number)-1
				end if
			end do
			
		end do
	end do

end do

!end timer
call CPU_Time(finish)

!print time elapsed
print*," "
call print_interval(start,finish)

end program in_search_of_sanity
