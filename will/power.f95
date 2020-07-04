program power
use flesh_out
implicit none

!network read/write
integer, allocatable :: brain(:,:,:)
real,allocatable :: blood(:,:,:)
integer :: thrash,active_data,grave
character(len=1000) :: maxim_column_cha,maxim_row_cha

!network dimension
integer :: maxim_row,maxim_column,here_row,here_column,ages,special

!condition options
logical :: only_neuron

!prepare command line options
IF(COMMAND_ARGUMENT_COUNT().NE.2)THEN
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program maximum_columns maximum_rows'
	STOP
ENDIF

!set the column/row variables
CALL GET_COMMAND_ARGUMENT(1,maxim_column_cha)
CALL GET_COMMAND_ARGUMENT(2,maxim_row_cha)
READ(maxim_column_cha,*)maxim_column
READ(maxim_row_cha,*)maxim_row

!allocate the networks
allocate(blood(maxim_row*maxim_column,maxim_column,maxim_row))
allocate(brain(1:maxim_column*maxim_row+2*(maxim_column+maxim_row)-4,1:maxim_column,1:maxim_row))

!retrieve previous network and move ahead thrash counter
open(unit=1,file="neurotic.txt")
do here_row=1,size(blood(1,1,:))
	do here_column=1,size(blood(1,:,1))
		read(1,*) blood(:,here_column,here_row)
	end do
end do
do here_row=1,size(brain(1,1,:))
	do here_column=1,size(brain(1,:,1))
		read(1,*) brain(:,here_column,here_row)
	end do
end do
read(1,*) thrash
read(1,*) active_data
read(1,*) grave
close(1)	




!test brain injection
if (brain(self_pos_brain(1,maxim_column/2,maxim_column),maxim_column/2,1)==0) then
	brain(self_pos_brain(1,maxim_column/2,maxim_column),maxim_column/2,1)=1
end if

!simple tester
!if brain activates neuron in a corner or in the middle, blood will flow
!only selects one path (no multipe blood sources activated simultaneously)
!several neuron leeway in selection
!first, make sure no other neurons have data
only_neuron=.true.
do ages=1,maxim_column
	!skip neurons to be counted
	if ((ages/=1) .and. (ages/=maxim_column/2) .and. (ages/=maxim_column)) then
		!if there is a brain neuron other than the ones designated that has a 1, set to false
		if (brain(self_pos_brain(maxim_row,ages,maxim_column),ages,maxim_row)==1) then
			only_neuron=.false.
		end if
	end if
end do
!now enact the blood boost if conditions are met
do ages=1,6
	if (thrash>(10*ages)) then
	
		!bottom left
		if (brain(self_pos_brain(maxim_row,1,size(blood(1,:,1))),1,maxim_row)==1) then
			!ignore first three neurons
			do special=4,maxim_column
				!if there is a brain neuron other than the ones designated that has a 1, set to false
				if (brain(self_pos_brain(maxim_row,special,maxim_column),special,maxim_row)==1) then
					only_neuron=.false.
				end if
			end do
			!if condition satisfied, let 'er rip
			if (only_neuron==.true.) then
				blood(self_pos_blood(maxim_row,1,maxim_column),1,maxim_row)=blood(self_pos_blood(maxim_row,1,maxim_column),1,maxim_row)+0.00001*(10**ages)
			end if
			!reset neuron tester
			only_neuron=.true.
		end if
		
		!bottom middle
		if (brain(self_pos_brain(maxim_row,maxim_column/2,size(blood(1,:,1))),maxim_column/2,maxim_row)==1) then
			do special=1,maxim_column
				!ignore middle three neurons
				if (abs((maxim_column/2)-special)<=1) then
					cycle
				end if
				!if there is a brain neuron other than the ones designated that has a 1, set to false
				if (brain(self_pos_brain(maxim_row,special,maxim_column),special,maxim_row)==1) then
					only_neuron=.false.
				end if
			end do
			!if condition satisfied, let 'er rip
			if (only_neuron==.true.) then
				blood(self_pos_blood(maxim_row,maxim_column/2,maxim_column),maxim_column/2,maxim_row)=&
					blood(self_pos_blood(maxim_row,maxim_column/2,maxim_column),maxim_column/2,maxim_row)+0.00001*(10**ages)
			end if
			!reset neuron tester
			only_neuron=.true.
		end if
		
		!bottom right
		if (brain(self_pos_brain(maxim_row,maxim_column,size(blood(1,:,1))),maxim_column,maxim_row)==1) then
			!ignore last three neurons
			do special=1,maxim_column-3
				!if there is a brain neuron other than the ones designated that has a 1, set to false
				if (brain(self_pos_brain(maxim_row,special,maxim_column),special,maxim_row)==1) then
					only_neuron=.false.
				end if
			end do
			!if condition satisfied, let 'er rip
			if (only_neuron==.true.) then
				blood(self_pos_blood(maxim_row,maxim_column,maxim_column),maxim_column,maxim_row)=&
					blood(self_pos_blood(maxim_row,maxim_column,maxim_column),maxim_column,maxim_row)+0.00001*(10**ages)
			end if

		end if
	end if
end do
	
	
	

!write the networks to file
open(unit=2,file="will.txt")
do here_row=1,size(blood(1,1,:))
	do here_column=1,size(blood(1,:,1))
		write(2,*) blood(:,here_column,here_row)
	end do
end do
do here_row=1,size(brain(1,1,:))
	do here_column=1,size(brain(1,:,1))
		write(2,*) brain(:,here_column,here_row)
	end do
end do
write(2,*) thrash
write(2,*) active_data
write(2,*) grave
close(2)

end program power
