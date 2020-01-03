program central_brain
use flesh
implicit none

!eyes, ears and nose are data inputs to be determined (RANDOM_NUMBER for now)
!time_interval to be used to time epoch to compute information degradation, total_time just to store total time of program execution

!timing objects
real ::  time_interval, start, finish, start_interval, finish_interval

!incrementation objects
integer :: epoch, epoch_number, j,i,z, f,u,k, s,a,c, pos_hold
integer,allocatable :: matrix_pos(:)
real :: fate, transition, distil, hope, fear
real,allocatable :: transition_list(:)

!main brain objects
integer :: maxim=0,maxim_hold,emerge_level_shape,emerge_mid,level_mid
integer,allocatable :: level_shape(:,:)
real,allocatable :: emerge(:,:,:)

!sense and brain stem objects
real,dimension(1) :: eyes, ears, nose
integer,parameter :: sense_sum=size(eyes)+size(ears)+size(nose)
real,dimension(sense_sum,sense_sum+1) :: brain_stem

!debugging objects
character(len=56) :: formatte="(F9.4,F9.4,F9.4,F9.4,F9.4,F9.4,F9.4,F9.4,F9.4,F9.4,F9.4)"
integer :: x









!----------------------------------
!          setup                  -
!----------------------------------


call CPU_Time(start)

!hard-coded length untill I can let this go free (run forever)
epoch_number=1000



!initial allocation of neuron space
!level_shape controls the size of a level (:)(value_left,value_right) for a certain level (row_pointer)(:) in the brain, thus note level size should always have 2 columns
allocate(level_shape(1:5,1:2))
level_shape(1,:)=[2,1]
level_shape(2,:)=[2,2]
level_shape(3,:)=[4,4]
level_shape(4,:)=[2,2]
level_shape(5,:)=[5,4]

!emerge is the cerebellum and is made with the maximum amout of columns needed to fit all the levels
!find the maximum level size
do i=1,size(level_shape(:,1))
	maxim_hold=2*maxval(level_shape(i,:))+1
	if (maxim_hold>maxim) then
		maxim=maxim_hold
	end if
end do

!allocate the maximum dimensions to the brain
!the first dimension is the number of levels (rows)
!the second dimension is the number of possible neurons in each level (column)
!the third dimension holds the data, (j,i,((j-1)*maxim+i)), and the weights, (j,i,z) for ((j-1)*maxim+i)/=z, as they relate to each of the other neurons
allocate(emerge(size(level_shape(:,1)),maxim,maxim*size(level_shape(:,1))))

!initialize the transition list that keeps track of transitions for weight altering
allocate(transition_list(1:size(emerge(1,1,:)-1)))

!initialise the randomised position marker arrays
allocate(matrix_pos(1:size(emerge(1,1,:))))


!find the midpoint of the emerge row
emerge_mid=(maxim/2)+1






!initialize the brain_stem weights
do s=1,size(brain_stem(:,1))
	do a=1,size(brain_stem(1,:))
		!list conditions for the brainstem
		if (s/=a) then
			!begin with equal probabilities for the brain stem
			brain_stem(s,a)=1.0/(size(brain_stem(:,1))*2)
		end if
	end do
end do
print formatte,0.,0.,0.,0.,brain_stem(1,1),brain_stem(2,2),brain_stem(3,3),0.,0.,0.,0.
print*,"Brain Stem"






!initialize the emerge weights
do s=1,size(emerge(:,1,1))
	do a=1,size(emerge(1,:,1))
		do c=1,size(emerge(1,1,:))
			!list shape conditions for the brain
			if ((emerge_mid-a<=level_shape(s,1)) .and. (emerge_mid-a>=-level_shape(s,2)) .and. (self_pos(s,a,maxim)/=c)) then
				!begin with equal probabilities on data transfer (change later)
				emerge(s,a,c)=1.0/(size(emerge(:,:,1))*2)
			!feed initial neurons with small data starter
			else if ((emerge_mid-a<=level_shape(s,1)) .and. (emerge_mid-a>=-level_shape(s,2)) .and. (self_pos(s,a,maxim)==c)) then
				emerge(s,a,c)=1.
			end if
		end do
	end do
	x=(s-1)*maxim
	print formatte,emerge(s,1,x+1),emerge(s,2,x+2),emerge(s,3,x+3),emerge(s,4,x+4),emerge(s,5,x+5),emerge(s,6,x+6),emerge(s,7,x+7),&
		emerge(s,8,x+8),emerge(s,9,x+9),emerge(s,10,x+10),emerge(s,11,x+11)
end do
print*,"round 0"
print*," "










!---------------------------------
!       brain activity           -
!---------------------------------


!main brain epoch operation
do epoch=1,epoch_number
	!record the start time of the epoch
	call CPU_Time(start_interval)
	
	!random generated input
	call RANDOM_NUMBER(eyes)
	call RANDOM_NUMBER(ears)
	call RANDOM_NUMBER(nose)
	!eyes=0
	!ears=0
	!nose=0
	!life is suffering
	brain_stem(1,1)=brain_stem(1,1)+eyes(1)
	brain_stem(2,2)=brain_stem(2,2)+ears(1)
	brain_stem(3,3)=brain_stem(3,3)+nose(1)

	!find the midpoint of the emerge row
	emerge_mid=(maxim/2)+1






	!brain stem has small network that feeds into emerge
	do j=1,size(brain_stem(:,1))
		do i=1,size(brain_stem(1,:))
			if ((j/=i) .and. (i/=sense_sum+1)) then
				!set the distance weight
				distil=exp(-(real(j)-real(i))**2)
				!set the value to be added to the j neuron from the targeted i neuron
				call RANDOM_NUMBER(fate)
				transition=brain_stem(i,i)*brain_stem(j,i)*(1.0-brain_stem(i,j))*fate*distil


				!print*,transition,j,i


				!this adds some weighted (0<weight<1) amount of data from the neuron targeted by i
				brain_stem(j,j)=brain_stem(j,j)+transition
				!this takes the requisite data away from the i neuron
				brain_stem(i,i)=brain_stem(i,i)-transition
			else if (i==sense_sum+1) then
				!this sends data from the brain stem to emerge
				f=emerge_mid-(size(brain_stem(:,1))/2)+j-1
				emerge(1,f,f)=emerge(1,f,f)+brain_stem(j,j)*brain_stem(j,i)
				!this takes away the amount of data, transferred to the emerge neuron, from the current one
				brain_stem(j,j)=brain_stem(j,j)*(1-brain_stem(j,i))
			end if
		end do
	end do
	print formatte,0.,0.,0.,0.,brain_stem(1,1),brain_stem(2,2),brain_stem(3,3),0.,0.,0.,0.
	print*,"Brain Stem"

	


	!the randomised loop initialiser - ensures data transition is not positionally dependant
	call randomised_list(matrix_pos)

	!reshape the emerge array as the brain grows beyond the current array bounds (to be written)
	
					





	!this is the brain neuron action loop (main loop)

	do s=1,size(emerge(1,1,:))
		!take the randomised array of matrix positions and select a neuron
		j=point_pos_matrix(matrix_pos(s),maxim,"row")
		i=point_pos_matrix(matrix_pos(s),maxim,"column")
		k=matrix_pos(s)	!k is the z position of the current matrix element represented by j and i

		!put dying neurons out of their misery
		if (emerge(j,i,k)<0.000) then
			emerge(j,i,k)=0.0
		end if		

		do z=1,size(emerge(1,1,:))
			
			!initialise random fire decision weights
			call RANDOM_NUMBER(hope)
			call RANDOM_NUMBER(fear)
			
			f=point_pos_matrix(z,maxim,"row")	!f is the j position of the current matrix element represented by z
			u=point_pos_matrix(z,maxim,"column")	!u is the i position of the current matrix element represented by z

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
		if ((j==3) .and. (i==6)) then
			print*,transition_list
			print*,emerge(j,i,:)
		end if

		!update the weights for this neuron based on the activity into the neuron
		call weight_change(emerge,j,i,k,transition_list)

	end do
	






	!print the brain
	do j=1,size(emerge(:,1,1))
		x=(j-1)*maxim
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
	do j=1,size(brain_stem(:,1))
		do i=1,size(brain_stem(1,:))
			emerge(j,i,self_pos(j,i,maxim))=emerge(j,i,self_pos(j,i,maxim))*(1-time_interval)
		end do
	end do
	
end do








!----------------------------
!          end timer        -
!----------------------------



call CPU_Time(finish)
call print_interval(start,finish)
end program
