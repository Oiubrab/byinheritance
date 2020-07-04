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

end module flesh_out
