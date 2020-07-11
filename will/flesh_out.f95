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

end module flesh_out
