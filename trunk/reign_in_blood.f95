module reign_in_blood
use welcome_to_dying
contains

!this module defines the pure motivation of the system
!it controls the fundamental motivation the motive network is acting on
!it is important to remember, the motive network is in charge, and it is
!the think network that is in service of it, not the other way around

!this subroutine applies a special weight to the motivate network based on the size of the binary number represented in it's input
!first argument must be the network and the second argument must be the input (vision) and the third argument must be the oddsey and the forth must be an image number
subroutine raining_blood(thinker,stinker,blinker,tinker)

	!inputs
	type(mind) :: thinker
	integer,dimension(*) :: stinker(:)
	integer :: blinker,tinker
	
	if (binary_to_decimal(stinker)<0) then 
		blinker=binary_to_decimal(stinker)/10
	else
		blinker=binary_to_decimal(stinker)/2
	end if
	
end subroutine







!This subroutine takes the neurochem matricies saved in each network and the output of the motivate network and applies a scaling factor to the weights of each network
!first argument must be the network and the second argument must be the multiplier from the motive network (oddsey)
subroutine animus(meaning,oddyseus)

	type(mind) :: meaning
	integer :: oddyseus, row, column, to, from, ladder

	!add to weights based on the neurochem at that node and the scaling 
	do row=1,size(meaning%brain_weight(1,1,1,:))
		do column=1,size(meaning%brain_weight(1,1,:,1))
			do ladder=1,size(meaning%neurochem(1,:,1,1))
			
				!for each rung in the neurochem ladder, establish which weight is to be altered
				from=meaning%neurochem(1,ladder,column,row)
				to=meaning%neurochem(2,ladder,column,row)
				
				!exclude ladder rungs that haven't been set yet
				if ((to/=0) .and. (from/=0)) then 
					!exclude intentionally zeroed out weights
					if (meaning%brain_weight(to,from,column,row)/=0.0) then
					
						!add the weight
						!the lower the weight is on the ladder, the smaller the modulation
						meaning%brain_weight(to,from,column,row)=meaning%brain_weight(to,from,column,row)+&
							(size(meaning%neurochem(1,:,1,1))+1-ladder)*oddyseus
							
						!if weight manipulation reduces weight below 1.0, make it 1.0
						if (meaning%brain_weight(to,from,column,row)<1.0) then
							meaning%brain_weight(to,from,column,row)=1.0
						end if
						 
					end if
				end if	
				
			end do
		end do
	end do

end subroutine




end module
