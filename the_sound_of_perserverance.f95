module the_sound_of_perserverance
use welcome_to_dying
contains

!work, damn
!this module defines the pure motivation of the system
!it controls the fundamental motivation the motive network is acting on
!it is important to remember, the motive network is in charge, and it is
!the think network that is in service of it, not the other way around

!this subroutine applies a special weight to the motivate network based on the size of the binary number represented in it's input

subroutine flesh_and_the_power_it_holds(thinker,vision,oddsey,image)

	!inputs
	type(mind) :: thinker
	integer,dimension(*) :: vision(:)
	integer :: oddsey,image

	!for motivate sight
	if (image==1) then
		oddsey=findloc(vision,1,dim=1)
	end if

	
end subroutine



end module
