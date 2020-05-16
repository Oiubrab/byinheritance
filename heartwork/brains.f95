program central_brain
use flesh
use cudafor
implicit none

!eyes, ears and nose are data inputs to be determined (RANDOM_NUMBER for now)
!time_interval to be used to time epoch to compute information degradation, total_time just to store total time of program execution

!fundamental parameters
real,parameter :: pi=4*asin(1./sqrt(2.))

!timing objects
real ::  time_interval, start, finish, start_interval, finish_interval

!main brain objects
integer :: maxim_column=11,maxim_row=5,maxim_hold,emerge_level_shape,level_mid
real,allocatable :: emerge(:,:,:)


!learning task objects
real :: wave

!debugging objects
character(len=56) :: formatte="(F9.4,F9.4,F9.4,F9.4,F9.4,F9.4,F9.4,F9.4,F9.4,F9.4,F9.4)"
integer :: x

!incrementation objects
integer :: epoch, epoch_number, j,i,z, f,u,k, s,a,c, pos_hold
integer,allocatable :: matrix_pos(:)

real :: fate, transition, distil, hope, fear
real,allocatable :: transition_list(:)








!----------------------------------
!          setup                  -
!----------------------------------


call CPU_Time(start)

!hard-coded length untill I can let this go free (run forever)
epoch_number=20000



!initial allocation of neuron space


!allocate the maximum dimensions to the brain
!the first dimension is the number of levels (rows)
!the second dimension is the number of possible neurons in each level (column)
!the third dimension holds the data, (j,i,((j-1)*maxim_column+i)), and the weights, (j,i,z) for ((j-1)*maxim_column+i)/=z, as they relate to each of the other neurons

allocate(emerge(maxim_row,maxim_column,maxim_row*maxim_column))

!initialize the transition list that keeps track of transitions for weight altering
allocate(transition_list(1:size(emerge(1,1,:)-1)))

!initialise the randomised position marker arrays
allocate(matrix_pos(1:size(emerge(1,1,:))))


print*,"Brain"
!initialize the emerge weights
do s=1,size(emerge(:,1,1))
	do a=1,size(emerge(1,:,1))
		do c=1,size(emerge(1,1,:))
			emerge(s,a,c)=1.
		end do
	end do
	x=(s-1)*maxim_column
	print formatte,emerge(s,1,x+1),emerge(s,2,x+2),emerge(s,3,x+3),emerge(s,4,x+4),emerge(s,5,x+5),emerge(s,6,x+6),emerge(s,7,x+7),&
		emerge(s,8,x+8),emerge(s,9,x+9),emerge(s,10,x+10),emerge(s,11,x+11)
end do
print*,"round 0"
print*," "





!the world sits on a knife edge


!---------------------------------
!       brain activity           -
!---------------------------------


!main brain epoch operation
do epoch=1,epoch_number

	!simple tester
	if (epoch>10000) then
		emerge(1,1,1)=emerge(1,1,1)+0.1
	else if (epoch>5000) then
		emerge(1,1,1)=emerge(1,1,1)+0.01
	else if (epoch>2500) then
		emerge(1,1,1)=emerge(1,1,1)+0.001
	end if

	!record the start time of the epoch
	call CPU_Time(start_interval)




	!this is the brain neuron action loop (main loop)

	!the randomised loop initialiser - ensures data transition is not positionally dependant
	call randomised_list(matrix_pos)
	
	do s=1,size(emerge(1,1,:))
		!take the randomised array of matrix positions and select a neuron
		j=point_pos_matrix(matrix_pos(s),maxim_column,"row")
		i=point_pos_matrix(matrix_pos(s),maxim_column,"column")
		k=matrix_pos(s)	!k is the z position of the current matrix element represented by j and i

		!put dying neurons out of their misery
		if (emerge(j,i,k)<0.00001) then
			emerge(j,i,k)=0.0
		end if		

		do z=1,size(emerge(1,1,:))
			
			!initialise random fire decision weights
			call RANDOM_NUMBER(hope)
			call RANDOM_NUMBER(fear)
			
			f=point_pos_matrix(z,maxim_column,"row")	!f is the j position of the current matrix element represented by z
			u=point_pos_matrix(z,maxim_column,"column")	!u is the i position of the current matrix element represented by z

			!the first condition stops the neuron from acting on itself
			!the second condition skips dead neurons
			if ((matrix_pos(s)/=z) .and. (emerge(j,i,k)/=0.)) then

				call neuron_fire(emerge,f,u,k,j,i,z,transition_list)

			else
				!ensure non active neuron references and data entries record 0
				transition_list(z)=0.0
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
	print*,"Brain"
	do j=1,size(emerge(:,1,1))
		x=(j-1)*maxim_column
		print formatte,emerge(j,1,x+1),emerge(j,2,x+2),emerge(j,3,x+3),emerge(j,4,x+4),emerge(j,5,x+5),emerge(j,6,x+6),emerge(j,7,x+7),&
			emerge(j,8,x+8),emerge(j,9,x+9),emerge(j,10,x+10),emerge(j,11,x+11)
	end do

	!add neuron space to the emerge matrix if conditions met (to write)

	!activate more neurons in the matrix if conditions are met (to write)
	





	!print epoch
	print'(A6,I0)','round ',epoch
	print*," "
	
	!record the end of time epoch
	call CPU_time(finish_interval)
	time_interval=finish_interval-start_interval

	!enact time penalty on each neuron
	!this is a key part of the system's learning power
	!this ensures against runaway neuron growth and also limits growth of the brain past a point where neuron action takes too long
	do j=1,size(emerge(:,1,1))
		do i=1,size(emerge(1,:,1))
			do z=1,size(emerge(1,1,:))
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
