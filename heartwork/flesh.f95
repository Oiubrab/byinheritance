module flesh
use cudafor
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






!cannot use this with CUDA as it is a function. Here, equation is put directly into CUDA kernels
!this function takes in row/column coordinates and returns the data position of the coordinates
attributes(device,host) function self_pos(j,i,maximum) result(z)
	integer,intent(in),value :: j,i,maximum
	integer :: z

	z=((j-1)*maximum+i)

end function self_pos






!takes in a neuron position along the matrix (j,i,z) with a single number and gives it's position in (column,row) format
attributes(device,host) subroutine point_pos_matrix(z_point,high,poster)
	integer,intent(in) :: z_point, high
	integer :: i
	integer,dimension(2),intent(inout) :: poster

	!give options for the row or column
	poster(2)=ceiling(float(z_point)/float(high))
	poster(1)=z_point-(poster(2)-1)*high

end subroutine point_pos_matrix






!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        rung two          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!






attributes(global) subroutine electroviolence(brain,blood,scaling)

	integer,dimension(*),intent(in),device :: brain(:,:,:)
	real,dimension(*),intent(inout),device :: blood(:,:,:)
	integer :: i,j,k,k_adj,addup,l,m
	integer,dimension(2),shared :: m_l
	real :: distance,distort,addup_sig,shock,activation=2.
	real,intent(in),value :: scaling

	i = blockDim%x * (blockIdx%x - 1) + threadIdx%x
	j = blockDim%y * (blockIdx%y - 1) + threadIdx%y
	
	if ((size(blood(1,1,:))>=i) .and. (size(blood(1,:,1))>=j)) then

		!set k for brain
		k_adj=self_pos(i,j,size(blood(1,:,1)))+1+size(brain(1,:,1))+i*2
		
		if (brain(k_adj,j,i)==1) then
			
			!initialise the summing variable
			addup=0
			
			if ((j/=1) .and. (i/=1)) then
				addup=addup+brain(k_adj,j-1,i-1)
			end if
			if (i/=1) then
				addup=addup+brain(k_adj,j,i-1)
			end if
			if ((j/=size(brain(1,:,1))) .and. (i/=1)) then
				addup=addup+brain(k_adj,j+1,i-1)
			end if
			if (j/=1) then
				addup=addup+brain(k_adj,j-1,i)
			end if
			if (j/=size(brain(1,:,1))) then
				addup=addup+brain(k_adj,j+1,i)
			end if
			if ((j/=1) .and. (i/=size(brain(1,1,:)))) then
				addup=addup+brain(k_adj,j-1,i+1)
			end if
			if (i/=size(brain(1,1,:))) then
				addup=addup+brain(k_adj,j,i+1)
			end if
			if ((j/=size(brain(1,:,1))) .and. (i/=size(brain(1,1,:)))) then
				addup=addup+brain(k_adj,j+1,i+1)
			end if
			
			!take data from other vessels and place it in this one
			do k=1,size(blood(:,1,1))
				if (k/=self_pos(i,j,size(blood(1,:,1)))) then
				
					call point_pos_matrix(k,size(blood(1,:,1)),m_l)
					distance=sqrt((float(m_l(2)-i)**2)+(float(m_l(1)-j)**2))
					distort=exp(-(distance)**2)
					addup_sig=1./(1.+exp(-(1./activation)*float(addup)))
					shock=distort*addup_sig*blood(k,m_l(1),m_l(2))*scaling
				
					if (shock<blood(k,m_l(1),m_l(2))) then
						!print*,"fu"
						blood(self_pos(i,j,size(blood(1,:,1))),j,i)=blood(self_pos(i,j,size(blood(1,:,1))),j,i)+shock
						blood(k,m_l(1),m_l(2))=blood(k,m_l(1),m_l(2))-shock
					else
						blood(self_pos(i,j,size(blood(1,:,1))),j,i)=blood(self_pos(i,j,size(blood(1,:,1))),j,i)+blood(k,m_l(1),m_l(2))
						blood(k,m_l(1),m_l(2))=0.
					end if
				end if
				
			end do

		end if
			
	end if

end subroutine electroviolence






!this subroutine tranfers data between neurons, with transfer depending on the relative weights between neurons and random factors
attributes(global) subroutine neuron_fire(blood,matrix_pos,hope,fear,fate)

	real :: transition,distil,dist,hold_unsig
	real,value :: fate,fear,hope
	real,dimension(*),intent(inout) :: blood(:,:,:)
	integer,dimension(*),intent(in) :: matrix_pos(:)
	integer,dimension(2) :: i_k,u_z
	integer :: f,u,k,j,i,z,s,s_max,f_max
	
	!setup the gpu variables
	s_max=size(blood(:,1,1)); f_max=size(blood(:,1,1))
	
	s = blockDim%x * (blockIdx%x - 1) + threadIdx%x
	f = blockDim%y * (blockIdx%y - 1) + threadIdx%y
	
	if ((s_max>=s) .and. (f_max>=f)) then

		!take the randomised array of matrix positions and select a neuron
		call point_pos_matrix(matrix_pos(s),size(blood(1,:,1)),i_k)
		j=matrix_pos(s)	!j is the z position of the current matrix element represented by i and k	
		
		!f is the j position of the current matrix element represented by z
		call point_pos_matrix(f,size(blood(1,:,1)),u_z)	!u is the i position of the current matrix element represented by z

		!the first condition stops the neuron from acting on itself
		!the second condition skips dead neurons
		if ((j/=f) .and. (blood(j,i,k)/=0.)) then	

			!here the neuron data transition is operated
						
			!use the distance between the neurons and weight accordingly
			dist=sqrt((real(i_k(2)-u_z(2))**2)+(real(u_z(1)-i_k(1))**2))
			!sigmoid wrapped in a gaussian
			distil=exp(-(dist*((1./(1.+exp(-blood(j,i_k(1),i_k(2)))))**(-1.)))**2)
			!data element of the z neuron * distance * sigmoid goverened by weights and random numbers
			transition=blood(f,u_z(1),u_z(2))*distil*(1./(1.+exp(-((hope*blood(f,i_k(1),i_k(2))-fear*blood(j,u_z(1),u_z(2)))))))

			!check if the operation will drain more than the origin neuron has
			if (transition<blood(f,u_z(1),u_z(2))) then
						
				!the equation below is: blood(entry holding data for this position) = blood(entry holding data for this position) + amount of data from the z neuron
				blood(j,i_k(1),i_k(2))=blood(j,i_k(1),i_k(2))+transition

				!this takes away the transition amount of data, transferred to the current neuron, from the z neuron
				blood(f,u_z(1),u_z(2))=blood(f,u_z(1),u_z(2))-transition
			
				!otherwise, drain neuron dry (further neuron death algorithm to come)
			else if (transition>=blood(f,u_z(1),u_z(2))) then
				!all of the data from the z neuron is taken
				blood(j,i_k(1),i_k(2))=blood(j,i_k(1),i_k(2))+blood(f,u_z(1),u_z(2))
			
				!this takes away all the data, transferred to the current neuron, from the z neuron
				blood(f,u_z(1),u_z(2))=0.0
			end if

			!invert the blood value (sigmoid), add the transition, then return the value to it's sigmoided state. This is for changing weights
			hold_unsig=transition-log((1./blood(f,i_k(1),i_k(2)))-1.)
			blood(f,i_k(1),i_k(2))=1./(1.+exp(-hold_unsig))

		end if
		
	end if


end subroutine neuron_fire






end module flesh
