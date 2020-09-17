program in_search_of_sanity
use welcome_to_dying
implicit none


!network setup
integer,parameter :: info_ports=66 !first 64 are weights, 65 the origin address of data (if present), 66 is data port
integer :: rows=10, columns=15
integer, allocatable :: brain(:,:,:), blood(:,:,:)

!sensing and response setup
integer, allocatable :: vision(:), response(:)
integer,parameter :: data_rate=200
real :: look !test variable for randomisiong vision array
character(len=6) :: opener="bottom"

!selecting and moving
integer :: row_number, column_number, row_number_2, column_number_2, info_number, row_random_number, column_random_number
integer,allocatable :: column_random(:),row_random(:)
integer :: moves=0, epoch, epoch_total=1000

!risk and reward
integer :: use_reward=30, blood_gradient=30

!timing
real :: start, finish, delay_time=0.05

!printing
integer,parameter :: individual_width=2, separation_space=10
character(len=individual_width) :: data_cha
character(len=12) :: individual_width_cha,separation_cha
character(len=17) :: width,width_separation
character(len=:),allocatable :: print_row



!start timer
call CPU_Time(start)

!allocation block
allocate(vision(columns)) !allocate the array for input into the network, currently on top
allocate(response(columns)) !allocate the array for output from the network, currently on bottom
allocate(brain(info_ports,columns,rows)) !allocate the network variable
allocate(blood(info_ports,columns,rows)) !allocate the reward variable
allocate(column_random(columns)) !allocate the column selection randomiser (randomised at main loop start)
allocate(row_random(rows)) !allocate the row selection randomiser (randomised at main loop start)


!initialise the network
call initialiser(brain,blood,vision,response,opener)

!test injection - an origin must accompany the data
vision(2)=1 !change this to detect the food position when I attach this to the game
vision(14)=1
do column_number=1,columns
	if (vision(column_number)==1) then	
		brain(info_ports-1,column_number,1)=2
		brain(info_ports,column_number,1)=1
	end if
end do


print*,"Brain moves: 0 Epoch: 0"
call print_network(brain,blood,vision,response)

!this is the new song
do epoch=1,epoch_total

	!injection from vision into brain
	if (mod(epoch,data_rate)==0) then
		do column_number=1,columns
			if (vision(column_number)==1) then	
				brain(info_ports-1,column_number,1)=2 !an origin must accompany the data
				brain(info_ports,column_number,1)=1
			end if
		end do
	end if

	!test - randomise vision
	vision=0
	call random_number(look)
	vision(int(look*float(columns))+1)=1
	call random_number(look)
	vision(int(look*float(columns))+1)=1

	!first, randomise random column and row lists
	call randomised_list(row_random)
	call randomised_list(column_random)
	 
	!now I shall send randomly selected neurons to have data moved
	do row_number=1,rows
		do column_number=1,columns
		
			column_random_number=column_random(column_number)
			row_random_number=row_random(row_number)
			
			!irrespective, add the gradient
			if (row_random_number==rows) then
				blood(info_ports,column_random_number,row_random_number)=blood_gradient
			else if (row_random_number==1) then
				blood(info_ports,column_random_number,row_random_number)=1
			end if
			
			!move the blood around
			if (blood(info_ports,column_random_number,row_random_number)>1) then
				call blood_mover(blood,column_random_number,row_random_number)
			end if
			
			!only act on neurons that have data in them
			if (brain(info_ports,column_random_number,row_random_number)==1) then
			
				!here is the important subroutine call that moves the data depending on how fat the neuron is 
				!individual data may move several times each loop. Loop these loops for a truly random movement (feature, not bug) 
				call selector(blood,brain,column_random_number,row_random_number,use_reward,response)
				
				!the choosing weights reduce by one if they are not used and increase by use_reward-1 if they are used
				!this is done by subtracting one from all weights and adding use_reward to weights that are used
				do info_number=1, info_ports-2
					if (brain(info_number,column_random_number,row_random_number)>blood(info_ports,column_random_number,row_random_number)) then
						brain(info_number,column_random_number,row_random_number)=&
							brain(info_number,column_random_number,row_random_number)-1
					end if
				end do
				
				!lag if necessary
				call delay(delay_time)
				
				!increase the moves count
				moves=moves+1

				
				!print each step
				print'(A15,I0,A8,I0)',"Brain moves: ",moves,"Epoch: ",epoch
				call print_network(brain,blood,vision,response)
				
			end if
			
		end do
	end do

end do

!end timer
call CPU_Time(finish)

!print time elapsed
print*," "
call print_interval(start,finish)

end program in_search_of_sanity
