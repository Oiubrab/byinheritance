program in_search_of_sanity
use bobokittyfuck
implicit none


!network
integer,parameter :: info_ports=66 !first 64 are weights, 65 the origin address of data (if present), 66 is data port
integer :: rows=10, columns=20
integer :: row_number, column_number, row_number_2, column_number_2, info_number, row_random_number, column_random_number
integer, allocatable :: brain(:,:,:)

!selecting and moving
integer,allocatable :: column_random(:),row_random(:)
integer :: moves=0, epoch, epoch_total=1000

!timing
real :: start, finish

!printing
integer,parameter :: individual_width=2
character(len=individual_width) :: data_cha
character(len=12) :: individual_width_cha
character(len=17) :: width
character(len=:),allocatable :: print_row



!start timer
call CPU_Time(start)

!allocation block
allocate(brain(info_ports,columns,rows)) !allocate the network variable
allocate(character(columns*individual_width+1) :: print_row) !allocate the printing variable
allocate(column_random(columns)) !allocate the column selection randomiser (randomised at main loop start)
allocate(row_random(rows)) !allocate the row selection randomiser (randomised at main loop start)

!set the width to print for each datum
write(individual_width_cha,*)individual_width
width="(I"//trim(individual_width_cha)//")"

!initialise the network
call initialiser(brain)

!test injection - an origin must accompany the data
brain(info_ports-1,columns/2,rows/2)=2
brain(info_ports,columns/2,rows/2)=1
brain(info_ports-1,(columns/2)-1,rows/2)=2
brain(info_ports,(columns/2)-1,rows/2)=1

print*,"Brain moves: 0 Epoch: 0"
do row_number=1,rows
	do column_number=1,columns
		write(data_cha,width)brain(info_ports,column_number,row_number)
		print_row(column_number*individual_width-(individual_width-1):column_number*individual_width)=data_cha
	end do
	print *,print_row
end do

!this is the new song
do epoch=1,epoch_total

	!first, randomise random column and row lists
	call randomised_list(row_random)
	call randomised_list(column_random)
	 
	!now I shall send randomly selected neurons to have data moved
	do row_number=1,rows
		do column_number=1,columns
		
			column_random_number=column_random(column_number)
			row_random_number=row_random(row_number)
			
			!only act on neurons that have data in them
			if (brain(info_ports,column_random_number,row_random_number)==1) then
			
				!here is the important subroutine call that moves the data depending on how fat the neuron is 
				!individual data may move several times each loop. Loop these loops for a truly random movement (feature, not bug) 
				call selector(brain,column_random_number,row_random_number)
				
				!lag if necessary
				call delay(0.1)
				
				!increase the moves count
				moves=moves+1
				
				!print each step
				print'(A14,I0,A8,I0)',"Brain moves: ",moves,"Epoch: ",epoch
				do row_number_2=1,rows
					do column_number_2=1,columns
						write(data_cha,width)brain(info_ports,column_number_2,row_number_2)
						print_row(column_number_2*individual_width-(individual_width-1):column_number_2*individual_width)=data_cha
					end do
					print *,print_row
				end do
				
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
