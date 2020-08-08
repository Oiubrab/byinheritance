module flesh_out
implicit none
contains


!this is the reward function - approach 1: general reward for staying on point
subroutine reward(impulse_action,impulse_input,vein_action)

	integer,dimension(*),intent(inout) :: impulse_action(:),impulse_input(:)
	real,dimension(*),intent(inout) :: vein_action(:)
	integer :: here_column,there_column
	
	!add blood to the vein_action neuron if there is data in the impulse_action neuron
	do here_column=1,size(impulse_action)
		!base diffusion rate draws data into the action
		!vein_action(here_column)=vein_action(here_column)+0.1
		vein_action(here_column)=vein_action(here_column)+5.0
		!add the blood, depending on the desired reward
		there_column=findloc(impulse_input,1,dim=1)
		vein_action(here_column)=vein_action(here_column)+5.0*(1.0/(float(abs(((size(impulse_input)/2)+1)-there_column))+1.0))

	end do
	
end subroutine reward



end module flesh_out
