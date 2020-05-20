program blood
use flesh
use cudafor
implicit none

!eyes, ears and nose are data inputs to be determined (RANDOM_NUMBER for now)
!time_interval to be used to time epoch to compute information degradation, total_time just to store total time of program execution

!fundamental parameters
real,parameter :: pi=4*asin(1./sqrt(2.))

!input variables
character(len=10000) :: maxim_column_cha,maxim_row_cha,lag_cha,epoch_number_cha

!timing objects
real ::  time_interval, start, finish, start_interval, finish_interval

!main brain objects
integer :: maxim_column,maxim_row
real,allocatable :: emerge(:,:,:)

!debugging objects
integer :: x

!incrementation objects
integer :: epoch, epoch_number, j,i,z, f,u,k, s,a,c, pos_hold, lag
integer,allocatable :: matrix_pos(:)

real :: fate, transition, distil
real,allocatable :: transition_list(:)

!printing objects
integer :: print_length=7
character(len=print_length) :: data_cha
character(len=:),allocatable :: print_row

!prepare command line options
IF(COMMAND_ARGUMENT_COUNT().NE.4)THEN
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program epoch_number maximum_columns, maximum_rows lag'
	STOP
ENDIF
!set the column/row variables
CALL GET_COMMAND_ARGUMENT(1,epoch_number_cha)
CALL GET_COMMAND_ARGUMENT(2,maxim_column_cha)
CALL GET_COMMAND_ARGUMENT(3,maxim_row_cha)
CALL GET_COMMAND_ARGUMENT(4,lag_cha)
READ(epoch_number_cha,*)epoch_number
READ(maxim_column_cha,*)maxim_column
READ(maxim_row_cha,*)maxim_row
READ(lag_cha,*)lag





!----------------------------------
!          setup                  -
!----------------------------------


call CPU_Time(start)


!allocate the maximum dimensions to the brain
!the first dimension is the number of levels (rows)
!the second dimension is the number of possible neurons in each level (column)
!the third dimension holds the data, (j,i,((j-1)*maxim_column+i)), and the weights, (j,i,z) for ((j-1)*maxim_column+i)/=z, as they relate to each of the other neurons

allocate(emerge(maxim_row*maxim_column,maxim_column,maxim_row))

!initialize the transition list that keeps track of transitions for weight altering
allocate(transition_list(1:size(emerge(:,1,1)-1)))

!initialise the randomised position marker arrays
allocate(matrix_pos(1:size(emerge(:,1,1))))

!initialise printer
allocate(character(maxim_column*print_length+7) :: print_row)

!initialize the emerge weights
do s=1,size(emerge(1,1,:))
	do a=1,size(emerge(1,:,1))
		do c=1,size(emerge(:,1,1))
			emerge(c,a,s)=1.
		end do
	end do
end do

!print the initial brain
print'(A7)',"Brain 0"
do s=1,size(emerge(1,1,:))
	do a=1,size(emerge(1,:,1))

		write(data_cha,"(F7.3)")emerge(self_pos(s,a,maxim_column),a,s)
		print_row(a*print_length:a*print_length+7)=data_cha
		
	end do
	print *,print_row

end do
print*," "




!the world sits on a knife edge


!---------------------------------
!       brain activity           -
!---------------------------------


!main brain epoch operation
do epoch=1,epoch_number

	call sleep(lag)

	!simple tester
	do x=1,8
		if (epoch>(50*x)) then
			emerge(1,1,1)=emerge(1,1,1)+0.00001*(10**x)
			emerge(self_pos(maxim_row,1,maxim_column),1,maxim_row)=emerge(self_pos(maxim_row,1,maxim_column),1,maxim_row)++0.00001*(10**x)
						emerge(self_pos(maxim_row/2,1,maxim_column),1,maxim_row/2)=emerge(self_pos(maxim_row/2,1,maxim_column),1,maxim_row/2)++0.00001*(10**x)
		end if
	end do

	!record the start time of the epoch
	call CPU_Time(start_interval)




	!this is the brain neuron action loop (main loop)

	!the randomised loop initialiser - ensures data transition is not positionally dependant
	call randomised_list(matrix_pos)
	
	do s=1,size(emerge(:,1,1))
		!take the randomised array of matrix positions and select a neuron
		k=point_pos_matrix(matrix_pos(s),maxim_column,"row")
		i=point_pos_matrix(matrix_pos(s),maxim_column,"column")
		j=matrix_pos(s)	!k is the z position of the current matrix element represented by j and i	

		do f=1,size(emerge(:,1,1))
		
			z=point_pos_matrix(f,maxim_column,"row")	!f is the j position of the current matrix element represented by z
			u=point_pos_matrix(f,maxim_column,"column")	!u is the i position of the current matrix element represented by z

			!the first condition stops the neuron from acting on itself
			!the second condition skips dead neurons
			if ((j/=f) .and. (emerge(j,i,k)/=0.)) then

				call neuron_fire(emerge,f,u,k,j,i,z,transition_list)

			else
				!ensure non active neuron references and data entries record 0
				transition_list(f)=0.0
			end if
		end do

		!track a neuron
		!if ((j==3) .and. (i==6)) then
		!	print*,transition_list
		!	print*,emerge(j,i,:)
		!end if

		!update the weights for this neuron based on the activity into the neuron
		call weight_change(emerge,j,i,k,transition_list)

	end do
	






	!print the brain
	print'(A6,I0)',"Brain ",epoch
	do s=1,size(emerge(1,1,:))
		do a=1,size(emerge(1,:,1))

			write(data_cha,"(F7.3)")emerge(self_pos(s,a,maxim_column),a,s)
			print_row(a*print_length:a*print_length+7)=data_cha
			
		end do
		print *,print_row

	end do
	print*," "

	!pass to neurotic

	!enact time penalty on each neuron
	!this is a key part of the system's learning power
	!this ensures against runaway neuron growth and also limits growth of the brain past a point where neuron action takes too long

	do z=1,size(emerge(1,1,:))
		do i=1,size(emerge(1,:,1))
			do j=1,size(emerge(:,1,1))
				emerge(j,i,z)=emerge(j,i,z)*(1-time_interval)
			end do
		end do
	end do
	
end do








!----------------------------
!          end timer        -
!----------------------------



call CPU_Time(finish)
call print_interval(start,finish)
end program
