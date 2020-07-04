program bloody
use flesh
use cudafor
implicit none

!eyes, ears and nose are data inputs to be determined (RANDOM_NUMBER for now)
!time_interval to be used to time epoch to compute information degradation, total_time just to store total time of program execution

!fundamental parameters
real,parameter :: pi=4*asin(1./sqrt(2.))

!input variables
character(len=10000) :: maxim_column_cha,maxim_row_cha,printed,multiplier_scaling_cha

!timing objects
real ::  time_interval, start, finish, start_interval, finish_interval

!main brain objects
integer :: maxim_column,maxim_row
real,allocatable :: blood(:,:,:)

!network objects
integer,allocatable :: brain(:,:,:)
integer :: active_data,grave

!debugging objects
integer :: x

!incrementation objects
integer :: epoch, j,i,z, f,u,k, s,a,c, pos_hold, here_weight,here_column,here_row, there_weight,there_column,there_row
integer,allocatable :: matrix_pos(:)

real :: fate, transition, distil,multiplier_scaling

!printing objects
integer :: print_length=7
character(len=print_length) :: data_cha
character(len=:),allocatable :: print_row

!file checking
logical :: file_exists

!make this a new run
call random_seed()

!prepare command line options
IF(COMMAND_ARGUMENT_COUNT().NE.4)THEN
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program maximum_columns maximum_rows scaling printed'
	WRITE(*,*)'printed: yes no'
	STOP
ENDIF

!set the column/row variables
CALL GET_COMMAND_ARGUMENT(1,maxim_column_cha)
CALL GET_COMMAND_ARGUMENT(2,maxim_row_cha)
CALL GET_COMMAND_ARGUMENT(3,multiplier_scaling_cha)
CALL GET_COMMAND_ARGUMENT(4,printed)
READ(multiplier_scaling_cha,*)multiplier_scaling
READ(maxim_column_cha,*)maxim_column
READ(maxim_row_cha,*)maxim_row

if ((printed/='no') .and. (printed/='yes') .and. (printed/='debug')) then
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program maximum_columns, maximum_rows scaling printed'
	WRITE(*,*)'printed: yes no'
	stop
end if





!----------------------------------
!          setup                  -
!----------------------------------


call CPU_Time(start)


!initialise the two networks
allocate(blood(maxim_row*maxim_column,maxim_column,maxim_row))
allocate(brain(1:maxim_column*maxim_row+2*(maxim_column+maxim_row)-4,1:maxim_column,1:maxim_row))

!initialise the randomised position marker arrays
allocate(matrix_pos(1:size(blood(:,1,1))))

!initialise printer
allocate(character(maxim_column*print_length+7) :: print_row)

!heartwork is the first network to run
!so we mst first test if this is the first epoch and initialize both brain and blood
INQUIRE(FILE="will.txt", EXIST=file_exists)
if (file_exists .eqv. .false.) then

	epoch=1

	!initialize the blood weights
	do s=1,size(blood(1,1,:))
		do a=1,size(blood(1,:,1))
			do c=1,size(blood(:,1,1))
				blood(c,a,s)=0.001
			end do
		end do
	end do
	
	!initialize the brain weights
	do s=1,size(brain(1,1,:))
		do a=1,size(brain(1,:,1))
			do c=1,size(brain(:,1,1))
				brain(c,a,s)=0
			end do
		end do
	end do

	!print the initial brain
	if (printed=="yes") then 
		print'(A7)',"Brain 0"
		do s=1,size(blood(1,1,:))
			do a=1,size(blood(1,:,1))

				write(data_cha,"(F7.3)")blood(self_pos(s,a,maxim_column),a,s)
				print_row(a*print_length:a*print_length+7)=data_cha
				
			end do
			print *,print_row

		end do
		print*," "

	end if




!the world sits on a knife edge


!---------------------------------
!       brain activity           -
!---------------------------------


!main brain epoch operation
else

	!retrieve previous network and move ahead epoch counter
	open(unit=1,file="will.txt")
	do s=1,size(blood(1,1,:))
		do a=1,size(blood(1,:,1))
			read(1,*) blood(:,a,s)
		end do
	end do
	do s=1,size(brain(1,1,:))
		do a=1,size(brain(1,:,1))
			read(1,*) brain(:,a,s)
		end do
	end do
	read(1,*) epoch
	read(1,*) active_data
	read(1,*) grave
	epoch=epoch+1
	close(1)	

	!affect the blood with the brain multipliers
	call electroviolence(brain,blood,multiplier_scaling)

	!record the start time of the epoch
	call CPU_Time(start_interval)




	!this is the brain neuron action loop (main loop)

	!the randomised loop initialiser - ensures data transition is not positionally dependant
	call randomised_list(matrix_pos)
	
	do s=1,size(blood(:,1,1))
		!take the randomised array of matrix positions and select a neuron
		here_row=point_pos_matrix(matrix_pos(s),maxim_column,"row")
		here_column=point_pos_matrix(matrix_pos(s),maxim_column,"column")
		here_weight=matrix_pos(s)	!k is the z position of the current matrix element represented by j and i	

		do there_weight=1,size(blood(:,1,1))
		
			there_row=point_pos_matrix(there_weight,maxim_column,"row")	!f is the j position of the current matrix element represented by z
			there_column=point_pos_matrix(there_weight,maxim_column,"column")	!u is the i position of the current matrix element represented by z

			!the first condition stops the neuron from acting on itself
			!the second condition skips dead neurons
			if ((here_weight/=there_weight) .and. (blood(here_weight,here_column,here_row)/=0.)) then

				call neuron_fire(blood,there_weight,there_column,here_row,here_weight,here_column,there_row)

			end if
		end do

	end do
	






	!print the brain
	if ((printed=="yes") .or. (printed=='debug')) then
		print'(A6,I0)',"Brain ",epoch
		do s=1,size(blood(1,1,:))
			do a=1,size(blood(1,:,1))

				write(data_cha,"(F7.3)")blood(self_pos(s,a,maxim_column),a,s)
				print_row(a*print_length:a*print_length+7)=data_cha
				
			end do
			print *,print_row

		end do
		print*," "
	
	end if

	!enact time penalty on each neuron
	!this is a key part of the system's learning power
	!this ensures against runaway neuron growth and also limits growth of the brain past a point where neuron action takes too long
	do z=1,size(blood(1,1,:))
		do i=1,size(blood(1,:,1))
			do j=1,size(blood(:,1,1))
				blood(j,i,z)=blood(j,i,z)*(1-time_interval)
			end do
		end do
	end do
	
end if




!save the networks to file
open(unit=2,file="heartwork.txt")
do s=1,size(blood(1,1,:))
	do a=1,size(blood(1,:,1))
		write(2,*) blood(:,a,s)
	end do
end do
do s=1,size(brain(1,1,:))
	do a=1,size(brain(1,:,1))
		write(2,*) brain(:,a,s)
	end do
end do
write(2,*) epoch
write(2,*) active_data
write(2,*) grave
close(2)


!----------------------------
!          end timer        -
!----------------------------



call CPU_Time(finish)
if ((printed=="yes") .or. (printed=='debug')) then
	call print_interval(start,finish)
end if
end program
