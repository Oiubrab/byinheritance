!network

!test:inject ones into matrix
!here this is done in a sweeping pattern, from left to right, then back to left

if ((-1)**((thrash/size(brain(1,:,1)))+1)==-1) then
	!add data to column
	brain(mod(thrash,maximum_columns)+4+maximum_columns,mod(thrash,size(brain(1,:,1)))+1,1)=1 !move from left to right
else
	!add data to column
	brain((maximum_columns*2+3)-mod(thrash,maximum_columns),&
		size(brain(1,:,1))-mod(thrash,size(brain(1,:,1))),1)=1 !move from right to left
end if	

if (brain(self_pos(1,maximum_columns/2,maximum_columns),maximum_columns/2,1)==0) then
	brain(self_pos(1,maximum_columns/2,maximum_columns),maximum_columns/2,1)=1
end if

!blood

	!simple tester
	do x=1,6
		if (epoch>(10*x)) then
			blood(self_pos(maxim_row,1,maxim_column),1,maxim_row)=blood(self_pos(maxim_row,1,maxim_column),1,maxim_row)+0.00001*(10**x)
			blood(self_pos(maxim_row,maxim_column/2,maxim_column),maxim_column/2,maxim_row)=&
				blood(self_pos(maxim_row,maxim_column/2,maxim_column),maxim_column/2,maxim_row)+0.00001*(10**x)
			blood(self_pos(maxim_row,maxim_column,maxim_column),maxim_column,maxim_row)=&
				blood(self_pos(maxim_row,maxim_column,maxim_column),maxim_column,maxim_row)+0.00001*(10**x)
		end if
	end do
