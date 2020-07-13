module flesh_out
implicit none
contains



!this function takes in row/column coordinates and returns the data position of the coordinates
function self_pos_blood(j,i,maximum) result(z)
	integer,intent(in) :: j,i,maximum
	integer :: z

	z=((j-1)*maximum+i)

end function self_pos_blood



!this function takes in row/column coordinates and returns the data position of the coordinates
function self_pos_brain(row,column,maximum_row_size) result(z)
	integer,intent(in) :: row,column,maximum_row_size
	integer :: z

	!finds the position in the local matrix
	z=((row-1)*maximum_row_size+column)

	!adds the correction for the matrix buffer
	z=z+(maximum_row_size+2)+row*2-1

end function self_pos_brain



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


!this is the reward function - approach 1: general reward for staying on point
subroutine reward(impulse_action,impulse_input,vein_action)

	integer,dimension(*),intent(inout) :: impulse_action(:),impulse_input(:)
	real,dimension(*),intent(inout) :: vein_action(:)
	integer :: here_column,there_column
	
	!add blood to the vein_action neuron if there is data in the impulse_action neuron
	do here_column=1,size(impulse_action)
		!base diffusion rate draws data into the action
		!vein_action(here_column)=vein_action(here_column)+0.1
		if (impulse_action(here_column)==1) then
			!vein_action(here_column)=vein_action(here_column)+5.0
			!add the blood, depending on the desired reward
			do there_column=1,size(impulse_input)
				if (impulse_input(there_column)==1) then
					!the closer the impulse input signal is to the middle, the bigger the reward
					print*,5.0*(float(abs(((size(impulse_input)/2)+1)-there_column))/float((size(impulse_input)/2)+1))
					vein_action(here_column)=vein_action(here_column)+5.0*(1.0/(float(abs(((size(impulse_input)/2)+1)-there_column))+1.0))
					exit
				end if
			end do
		end if
	end do
	
end subroutine reward



end module flesh_out
