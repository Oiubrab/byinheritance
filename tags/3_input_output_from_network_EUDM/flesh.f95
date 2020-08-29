module flesh
use emerge
implicit none
contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Each function or subroutine on a rung relies on functions or subroutines on the rung before it. !!
!! If a function/subroutine cannot function without the subroutines/functions on its rung          !!
!! (explicitly), or on rungs above it, then it should be moved up a rung.                          !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!        rung one          !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






















!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        rung two          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!





!this subroutine tranfers data between neurons, with transfer depending on the relative weights between neurons and random factors
subroutine neuron_fire(blood,there_weight,there_column,here_row,here_weight,here_column,there_row)

	real :: fear,hope,transition,distil,dist,hold_unsig
	real,dimension(*),intent(inout) :: blood(:,:,:)
	integer :: there_weight,there_column,here_row,here_weight,here_column,there_row
	

	!here the neuron data transition is operated
				
	!set the prospective transitionn value random multipliers
	call RANDOM_NUMBER(hope)
	call RANDOM_NUMBER(fear)
	!use the distance between the neurons and weight accordingly
	dist=sqrt((float(here_row-there_row)**2)+(float(here_column-there_column)**2))
	distil=exp(-(dist*(sigmoid(blood(here_weight,here_column,here_row),"forward")**(-1.)))**2)
	!data element of the z neuron * distance * sigmoid goverened by weights and random numbers
	transition=blood(there_weight,there_column,there_row)*distil*sigmoid((hope*blood(there_weight,here_column,here_row)&
		-fear*blood(here_weight,there_column,there_row)),"forward")			

	!check if the operation will drain more than the origin neuron has
	if (transition<blood(there_weight,there_column,there_row)) then
				
		!the equation below is: blood(entry holding data for this position) = blood(entry holding data for this position) + amount of data from the z neuron
		blood(here_weight,here_column,here_row)=blood(here_weight,here_column,here_row)+transition

		!this takes away the transition amount of data, transferred to the current neuron, from the z neuron
		blood(there_weight,there_column,there_row)=blood(there_weight,there_column,there_row)-transition
	
		!otherwise, drain neuron dry (further neuron death algorithm to come)
	else if (transition>=blood(there_weight,there_column,there_row)) then
		!all of the data from the z neuron is taken
		blood(here_weight,here_column,here_row)=blood(here_weight,here_column,here_row)+blood(there_weight,there_column,there_row)
	
		!this takes away all the data, transferred to the current neuron, from the z neuron
		blood(there_weight,there_column,there_row)=0.0
	end if
				
	hold_unsig=transition+sigmoid(blood(there_weight,here_column,here_row),"reverse")
	blood(there_weight,here_column,here_row)=sigmoid(hold_unsig,"forward")

end subroutine neuron_fire






subroutine electroviolence(brain,blood,scaling)

	integer,dimension(*),intent(in) :: brain(:,:,:)
	real,dimension(*),intent(inout) :: blood(:,:,:)
	integer :: i,j,k,k_brain,addup,l,m
	real :: distance,distort,addup_sig,shock
	real,intent(in) :: scaling
	
	do i=1,size(blood(1,1,:))
		do j=1,size(blood(1,:,1))

			!set k for brain
			k_brain=self_pos_brain(i,j,size(blood(1,:,1)))
			
			if (brain(k_brain,j,i)==1) then
				
				!initialise the summing variable
				addup=0
				
				if ((j/=1) .and. (i/=1)) then
					addup=addup+brain(k_brain,j-1,i-1)
				end if
				if (i/=1) then
					addup=addup+brain(k_brain,j,i-1)
				end if
				if ((j/=size(brain(1,:,1))) .and. (i/=1)) then
					addup=addup+brain(k_brain,j+1,i-1)
				end if
				if (j/=1) then
					addup=addup+brain(k_brain,j-1,i)
				end if
				if (j/=size(brain(1,:,1))) then
					addup=addup+brain(k_brain,j+1,i)
				end if
				if ((j/=1) .and. (i/=size(brain(1,1,:)))) then
					addup=addup+brain(k_brain,j-1,i+1)
				end if
				if (i/=size(brain(1,1,:))) then
					addup=addup+brain(k_brain,j,i+1)
				end if
				if ((j/=size(brain(1,:,1))) .and. (i/=size(brain(1,1,:)))) then
					addup=addup+brain(k_brain,j+1,i+1)
				end if
				
				!take data from other vessels and place it in this one
				do k=1,size(blood(:,1,1))
					if (k/=self_pos_blood(i,j,size(blood(1,:,1)))) then
					
						l=point_pos_matrix_blood(k,size(blood(1,:,1)),"row")
						m=point_pos_matrix_blood(k,size(blood(1,:,1)),"column")
						distance=sqrt((float(l-i)**2)+(float(m-j)**2))
						distort=exp(-(distance)**2)
						addup_sig=sigmoid(float(addup),"forward",domain_stretch=2.)
						shock=distort*addup_sig*blood(k,m,l)*scaling
					
						if (shock<blood(k,m,l)) then
							!print*,"fu"
							blood(self_pos_blood(i,j,size(blood(1,:,1))),j,i)=blood(self_pos_blood(i,j,size(blood(1,:,1))),j,i)+shock
							blood(k,m,l)=blood(k,m,l)-shock
						else
							blood(self_pos_blood(i,j,size(blood(1,:,1))),j,i)=blood(self_pos_blood(i,j,size(blood(1,:,1))),j,i)+blood(k,m,l)
							blood(k,m,l)=0.
						end if
					end if
					
				end do

			end if
			

		end do
	end do

end subroutine electroviolence





end module flesh
