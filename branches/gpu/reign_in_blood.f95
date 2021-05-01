module reign_in_blood
use welcome_to_dying
contains

!this module defines the pure motivation of the system
!it controls the fundamental motivation the motive network is acting on
!it is important to remember, the motive network is in charge, and it is
!the think network that is in service of it, not the other way around

!this subroutine applies a special weight to the motivate network based on the size of the binary number represented in it's input
!the first argument must be the input (vision) and the second argument must be the oddsey and the third must be an image number
pure subroutine raining_blood(stinker,blinker,tinker)

	!inputs
	type(mind) :: thinker
	integer,dimension(*),intent(in) :: stinker(:)
	integer,intent(inout) :: blinker
	integer,intent(in) :: tinker
	character(len=10) :: interpreter_type
	!$acc routine seq
	
	!this controls the type of interpreting done and must be set in aggreeance with the same setting in the test_market python script
	interpreter_type="positional"
	
	if (interpreter_type=="binary") then
	
		if (binary_to_decimal(stinker)<0) then 
			blinker=binary_to_decimal(stinker)/10
		else
			blinker=binary_to_decimal(stinker)/2
		end if
	
	else if (interpreter_type=="positional") then
	
		if (position_to_percentage(stinker)<0) then 
			blinker=position_to_percentage(stinker)/10
		else
			blinker=position_to_percentage(stinker)/2
		end if
		
	end if
	
end subroutine



end module
