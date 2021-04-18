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



end module
