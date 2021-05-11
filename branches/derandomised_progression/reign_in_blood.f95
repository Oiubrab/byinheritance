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

	!for motivate feel, convert back to a percentage and reward accordingly
	if (tinker==1) then
		if (position_to_percentage(stinker)<0) then 
			blinker=position_to_percentage(stinker)/10
		else
			blinker=position_to_percentage(stinker)/2
		end if
	!for motivate errors, motivate perfection and punish mistakes
	else if (tinker==2) then
		blinker=findloc(stinker,1,dim=1)
		if (blinker==1) then
			blinker=90
		else
			blinker=-30*blinker
		end if
	end if

	
end subroutine



end module
