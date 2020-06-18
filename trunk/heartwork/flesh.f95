module flesh
use cudafor
use curand
implicit none
contains

!each function or subroutine on a rung relies on functions or subroutines on the rung before it



!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        rung one          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!





!feed in a start and finish time for a time interval printout in hrs, mins, sec
subroutine print_interval(start,finish)
	real,intent(in) :: start, finish
	real :: t_sec, total_time
	integer :: t_hr, t_min
	
	total_time=finish-start
	t_hr = floor(total_time/3600)
	t_min = (total_time-t_hr*3600)/60
	t_sec = total_time-t_hr*3600-t_min*60
	print'(A14,I2,A5,I2,A7,F5.2,A4)',"time elapsed =",t_hr,' hrs, ',t_min,' mins, ',t_sec,' sec'

end subroutine print_interval






!returns a list of sequential numbers up to a length defined by the array input, in a list where the order of the numbers has been randomised
!master_killer is the allocatable array argument and world_eater is the array of known size
subroutine randomised_list(master_killer)

	integer,dimension(*),intent(inout) :: master_killer(:)
	integer :: roger_explosion, length, pos_hold
	real :: despair

		length=size(master_killer)

		!make sequential array of length defined by the array size
		do roger_explosion=1,length
			master_killer(roger_explosion)=roger_explosion
		end do
	
		!swap elements until the list is randomized
		do roger_explosion=1,length
			call RANDOM_NUMBER(despair)
			pos_hold=master_killer(roger_explosion)
			master_killer(roger_explosion)=master_killer(int(length*despair)+1)
			master_killer(int(length*despair)+1)=pos_hold
		end do

end subroutine randomised_list






!Here, equation is put directly into CUDA kernels
!this function takes in row/column coordinates and returns the data position of the coordinates
attributes(device,host) function self_pos(j,i,maximum) result(z)
	integer,intent(in),value :: j,i,maximum
	integer :: z

	z=((j-1)*maximum+i)

end function self_pos






!takes in a neuron position along the matrix (j,i,z) with a single number and gives it's position in (column,row) format
attributes(device,host) function point_pos_matrix(z_point,high) result(poster)
	integer,value,intent(in) :: z_point, high
	integer :: i
	integer,dimension(2) :: poster

	!give options for the row or column
	poster(2)=ceiling(float(z_point)/float(high))
	poster(1)=z_point-(poster(2)-1)*high

end function point_pos_matrix






!checks wether any entries in two arrays are equivalent
attributes(global) subroutine perform(master_blaster,wisdom)
	integer,dimension(*) :: master_blaster(:,:)
	logical :: wisdom
	integer :: check

	check = blockDim%x * (blockIdx%x - 1) + threadIdx%x
	
	if (check<=size(master_blaster(:,1))) then
		!if the values are equivalent, the condition is violated
		if (master_blaster(check,1)==master_blaster(check,2)) then
			wisdom=.false.
		end if
		
	end if
	
end subroutine perform






!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        rung two          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!






!sets up two arrays with no matching row partners for any column
subroutine randomised_sex(master_of_puppets)
	integer,dimension(*),intent(inout) :: master_of_puppets(:,:)
	integer,allocatable,device :: master_of_device(:,:)
	logical :: lies
	logical,device :: lies_device
	type(dim3) :: thread_per_block,block_per_grid
	
	!get ready
	lies=.false.
	allocate(master_of_device(size(master_of_puppets(:,1)),2))
	thread_per_block=dim3(16,1,1)
	block_per_grid=dim3(ceiling(float(size(master_of_puppets(:,1)))/thread_per_block%x),1,1)	
	
	do while (lies==.false.)
		lies=.true.
		call randomised_list(master_of_puppets(:,1))
		call randomised_list(master_of_puppets(:,2))
		
		master_of_device=master_of_puppets
		lies_device=lies
		call perform<<<block_per_grid,thread_per_block>>>(master_of_device,lies_device)
		master_of_puppets=master_of_device
		lies=lies_device
		
	end do
	
end subroutine randomised_sex






!for an individual neuron, takes the weights, data and peripherals for a particular neuron pair and stores a calculated transition
attributes(global) subroutine neuron_pre_fire_individual(bloodhound,blood,hope,local_column,local_row,violence)
	real,dimension(*),intent(in) :: hope(:,:,:)
	real,dimension(*),intent(in) :: blood(:,:,:)
	real,dimension(*),intent(inout) :: bloodhound(:,:,:,:)
	integer,value :: local_column,local_row
	real,value :: violence
	integer :: istat
	integer :: remote_weight !comes from thread establishment
	integer,dimension(2) :: bridge
	integer :: remote_column,remote_row,local_weight !derived from above integers
	real :: distil,dist,transition_sigmoid,sigmoid_content_pos,sigmoid_content_neg,sum_it_up,hold_unsig,electro,trans
	print*,"whyme"
	!get the remote_weight variable from the thread id
	remote_weight = blockDim%x * (blockIdx%x - 1) + threadIdx%x
	
	if (remote_weight<=size(blood(:,1,1))) then
		
		!now the fun begins
		!with the remotes first
		bridge=point_pos_matrix(remote_weight,size(blood(1,:,1)))
		remote_row=bridge(2)
		remote_column=bridge(1)
		
		!and now the locals come back into town
		local_weight=self_pos(local_row,local_column,size(blood(1,:,1)))
		
		!only run if there is data in the neuron
		if ((blood(local_weight,local_column,local_row)>0.) .and. blood(remote_weight,remote_column,remote_row)>0.) then
		
			!distance between local and remote
			dist=sqrt((float(local_column-remote_column)**2)+(float(local_row-remote_row)**2))
			!slap a gaussian on that
			distil=exp(-(dist**2))
			
			!sum up the neuron values
			sum_it_up=blood(local_weight,local_column,local_row)+blood(remote_weight,remote_column,remote_row)
			
			!this brings data in to the local neuron
			sigmoid_content_pos=hope(remote_weight,local_column,local_row)*blood(remote_weight,local_column,local_row)
			!this brings data out of the local neuron
			sigmoid_content_neg=hope(local_weight,remote_column,remote_row)*blood(local_weight,remote_column,remote_row)
			!this decides the weight and direction
			transition_sigmoid=2*(1/(1+exp(-(sigmoid_content_pos-sigmoid_content_neg))))-1
			!scaling brain with sigmoid touch
			electro=1/(1+exp(-(violence)))
			!finally, the transition value for this neuron, at this weight
			trans=transition_sigmoid*distil*sum_it_up*electro
			
			!Oh, wait. If transition is too big, one neuron gobbles out another
			if (trans>blood(remote_weight,remote_column,remote_row)) then
				
				trans=blood(remote_weight,remote_column,remote_row)
			
			else if (trans>blood(local_weight,local_column,local_row)) then
			
				trans=blood(local_weight,local_column,local_row)
			
			end if
		
			!without further ado, the transitions (1 is in and 2 is out)
			bloodhound(1,remote_weight,local_column,local_row)=trans
			bloodhound(2,local_weight,remote_column,remote_row)=-1.*trans	
		
		else
		
			bloodhound(1,remote_weight,local_column,local_row)=0.
			bloodhound(2,local_weight,remote_column,remote_row)=0.
		
		end if
		
	end if
	print*,"why"
	istat=cudaDeviceSynchronize()

end subroutine neuron_pre_fire_individual






!this subroutine takes the precalculated transitions between each neuron and enacts them, complete with weight change
subroutine random_neuron_fire(blood,blood_tranny,master_and_commander)
	real,dimension(*),intent(in) :: blood_tranny(:,:,:,:)
	real,dimension(*),intent(inout) :: blood(:,:,:)
	integer,dimension(*),intent(in) :: master_and_commander(:,:)
	integer,dimension(2) :: texas_ranger
	real :: in_out_here,in_out_there,hold_unsig
	integer :: here_weight,here_column,here_row,there_weight,there_column,there_row,walker
	
	do walker=1,size(blood(:,1,1))
	
		!get the here position
		here_weight=master_and_commander(walker,1)
		texas_ranger=point_pos_matrix(here_weight,size(blood(1,:,1)))
		here_row=texas_ranger(2)
		here_column=texas_ranger(1)
		
		!get the there position
		there_weight=master_and_commander(walker,2)
		texas_ranger=point_pos_matrix(there_weight,size(blood(1,:,1)))
		there_row=texas_ranger(2)
		there_column=texas_ranger(1)
		
		!calculate transition
		in_out_here=blood_tranny(1,here_weight,here_column,here_row)+blood_tranny(2,here_weight,here_column,here_row)
		in_out_there=blood_tranny(1,there_weight,there_column,there_row)+blood_tranny(2,there_weight,there_column,there_row)		
		
		!make sure neuron isn't drained past 0
		if (in_out_there+blood(there_weight,there_column,there_row)<0.) then
			in_out_there=blood(there_weight,there_column,there_row)
			in_out_here=-1.*in_out_there
		else if (in_out_here+blood(here_weight,here_column,here_row)<0.) then
			in_out_here=blood(here_weight,here_column,here_row)
			in_out_there=-1.*in_out_here
		end if
		
		!okay, make the transition
		blood(here_weight,here_column,here_row)=blood(here_weight,here_column,here_row)+in_out_here
		blood(there_weight,there_column,there_row)=blood(there_weight,there_column,there_row)+in_out_there
		
		!shift the weight that points from the local to the remote neuron
		hold_unsig=in_out_here-log((1./blood(there_weight,here_column,here_row))-1.)
		blood(there_weight,here_column,here_row)=1/(1+exp(-hold_unsig))
		
		!shift the weight that points from the remote to the local neuron
		hold_unsig=in_out_there-log((1./blood(here_weight,there_column,there_row))-1.)
		blood(here_weight,there_column,there_row)=1/(1+exp(-hold_unsig))		
		
	end do
end subroutine random_neuron_fire






!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        rung three        !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!






!for each thread, the value of neuron at each thread id is altered based on the aggregate affects of itself and the system
attributes(global) subroutine neuron_pre_fire(hope,blood,blood_transition,brain,multi_scaling)
	real,dimension(*),intent(in) :: hope(:,:,:)
	real,dimension(*),intent(inout) :: blood(:,:,:),blood_transition(:,:,:,:)
	integer,dimension(*),intent(in) :: brain(:,:,:)
	integer :: row,column,brain_weight,addup
	real :: dist,distil_down,distil_up,distil,electroviolence
	real,value :: multi_scaling
	integer :: istat
	type(dim3) :: thread_per_block,block_per_grid
	
	!establish which neurons the kernel is using
	column = blockDim%x * (blockIdx%x - 1) + threadIdx%x
	row = blockDim%y * (blockIdx%y - 1) + threadIdx%y
	
	if ((column<=size(blood(1,:,1))) .and. (row<=size(blood(1,1,:)))) then
	
		!set k for brain
		brain_weight=self_pos(row,column,size(blood(1,:,1)))+1+size(brain(1,:,1))+row*2
		
		!if brain has data, calculate weight multiplier derived from brain weights
		if (brain(brain_weight,column,row)==1) then
			
			!initialise the summing variable
			addup=0
			
			if ((column/=1) .and. (row/=1)) then
				addup=addup+brain(brain_weight,column-1,row-1)
			end if
			if (row/=1) then
				addup=addup+brain(brain_weight,column,row-1)
			end if
			if ((column/=size(brain(1,:,1))) .and. (row/=1)) then
				addup=addup+brain(brain_weight,column+1,row-1)
			end if
			if (column/=1) then
				addup=addup+brain(brain_weight,column-1,row)
			end if
			if (column/=size(brain(1,:,1))) then
				addup=addup+brain(brain_weight,column+1,row)
			end if
			if ((column/=1) .and. (row/=size(brain(1,1,:)))) then
				addup=addup+brain(brain_weight,column-1,row+1)
			end if
			if (row/=size(brain(1,1,:))) then
				addup=addup+brain(brain_weight,column,row+1)
			end if
			if ((column/=size(brain(1,:,1))) .and. (row/=size(brain(1,1,:)))) then
				addup=addup+brain(brain_weight,column+1,row+1)
			end if
			
			!make addup real then multiply by scaling
			electroviolence=float(addup)*multi_scaling
		
		else
		
			electroviolence=0.
			
		end if
	
		!get the neuron weight kernels ready
		thread_per_block=dim3(16,1,1)
		block_per_grid=dim3(ceiling(float(size(blood(:,1,1)))/thread_per_block%x),1,1)	
		print*,"die"
		call neuron_pre_fire_individual<<<block_per_grid, thread_per_block>>>(blood_transition,blood,hope,column,row,electroviolence)
		print*,"whynot"
		
	end if

end subroutine neuron_pre_fire
 
end module flesh
