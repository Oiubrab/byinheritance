program power
use flesh_out
implicit none

!network read/write
integer, allocatable :: brain(:,:,:)
real,allocatable :: blood(:,:,:)
integer :: thrash,active_data,grave
character(len=1000) :: maxim_column_cha,maxim_row_cha,network_scaling_cha,printed,cyclical_cha

!printing
character(len=:),allocatable :: print_row_impulse_action,print_row_impulse_input
character(len=2) :: data_cha_impulse_action,data_cha_impulse_input
integer :: print_length_vein_action=7
character(len=:),allocatable :: print_row_vein_action,data_cha_vein_action

!action objects
integer, allocatable :: impulse_action(:),impulse_input(:)
real, allocatable :: vein_action(:)
real :: transition,vein_change
integer :: shift,shift_total,shift_max,cyclical

!timing control
character(len=1000) :: lag_cha
integer :: lag
real :: start,finish

!network dimension
integer :: maxim_row,maxim_column,here_row,here_column,ages,special,there_column,input_here_column

!activation options
real :: scaling
logical :: switch1=.true.
logical :: switch2=.true.

!start timer
call cpu_time(start)

!prepare command line options
IF(COMMAND_ARGUMENT_COUNT().NE.6)THEN
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program cycles maximum_columns maximum_rows lag network_scaling printed'
	WRITE(*,*)'printed: yes no debug'
	STOP
ENDIF

!set the column/row variables
CALL GET_COMMAND_ARGUMENT(1,cyclical_cha)
CALL GET_COMMAND_ARGUMENT(2,maxim_column_cha)
CALL GET_COMMAND_ARGUMENT(3,maxim_row_cha)
CALL GET_COMMAND_ARGUMENT(4,lag_cha)
CALL GET_COMMAND_ARGUMENT(5,network_scaling_cha)
CALL GET_COMMAND_ARGUMENT(6,printed)
READ(cyclical_cha,*)cyclical
READ(lag_cha,*)lag
READ(network_scaling_cha,*)scaling
READ(maxim_column_cha,*)maxim_column
READ(maxim_row_cha,*)maxim_row

!stop if printed is not right
if ((printed/='no') .and. (printed/='yes') .and. (printed/='debug')) then
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program maximum_columns maximum_rows network_scaling printed'
	WRITE(*,*)'printed: yes no debug'
	STOP
ENDIF

!allocate the networks
allocate(blood(maxim_row*maxim_column,maxim_column,maxim_row))
allocate(brain((maxim_column+2)*(maxim_row+2),maxim_column,maxim_row))
allocate(vein_action(maxim_column+2))
allocate(impulse_action(maxim_column+2))
allocate(impulse_input(maxim_column))
allocate(character((maxim_column)*2+1) :: print_row_impulse_input)
allocate(character((maxim_column+2)*2+1) :: print_row_impulse_action)
allocate(character((maxim_column+2)*print_length_vein_action+7) :: print_row_vein_action)
allocate(character(print_length_vein_action) :: data_cha_vein_action)

!retrieve previous network
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
read(1,*) vein_action
read(1,*) impulse_action
read(1,*) impulse_input
read(1,*) thrash
read(1,*) active_data
read(1,*) grave
close(1)	

!test1: keep data in impulse_input in the centre (odd columns only)

!move the impulse_input based on the impulse_action
!at the start, set input in the middle
if (thrash<=1) then

	do here_column=1,size(impulse_input)
		impulse_input(here_column)=0
	end do 
	impulse_input((size(impulse_input)/2)+1)=1
	input_here_column=(size(impulse_input)/2)+1	
	shift=0
	
else if ((thrash<2000) .and. (mod(thrash,250)==0)) then

	do here_column=1,size(impulse_input)
		impulse_input(here_column)=0
	end do 
	impulse_input((size(impulse_input)/2)+1)=1
	input_here_column=(size(impulse_input)/2)+1
	shift=0

else if ((thrash>=2000) .and. (thrash<10000) .and. (mod(thrash,2000)==0)) then

	do here_column=1,size(impulse_input)
		impulse_input(here_column)=0
	end do 
	impulse_input(size(impulse_input))=1
	input_here_column=size(impulse_input)
	shift=0
	
else if ((thrash>=2000) .and. (thrash<10000) .and. (mod(thrash,1000)==0)) then

	do here_column=1,size(impulse_input)
		impulse_input(here_column)=0
	end do 
	impulse_input(1)=1
	input_here_column=1
	shift=0

else
	!set the input position
	input_here_column=findloc(impulse_input,1,dim=1)
	!initialise shifters
	shift_total=0
	shift_max=0
	!sum up distance and direction away from sentre impulse
	do here_column=1,size(impulse_action)
		if (impulse_action(here_column)==1) then
			shift_total=shift_total-((size(impulse_action)/2)+1-here_column)
		end if
		!setup shift max
		if (here_column<(size(impulse_action)/2)+1) then
			shift_max=shift_max+here_column
		end if
	end do
	!set how much the datum will shift by
	shift=int((float(shift_total)/float(shift_max))*float(size(impulse_input)-1))
	!try limit shifting
	if (shift>1) then
		shift=1
	else if (shift<-1) then
		shift=-1
	end if
	!shift the datum
	impulse_input(input_here_column)=0
	!don't let the datum leave the array
	if (input_here_column+shift<1) then
		impulse_input(1)=1
	else if (input_here_column+shift>size(impulse_input)) then
		impulse_input(size(impulse_input))=1
	else
		impulse_input(input_here_column+shift)=1
	end if
	!reset input position
	input_here_column=findloc(impulse_input,1,dim=1)
end if




!test brain injection
brain(self_pos_brain(1,input_here_column,maxim_column),input_here_column,1)=1

!dispense reward
!call reward(impulse_action,impulse_input,vein_action)

!reward function - approach 2:directly add to vein action on opposite side based on input - more hand holding
!vein_action(2*((size(impulse_input)/2)+1)-input_here_column+1)=vein_action(2*((size(impulse_input)/2)+1)-input_here_column+1)&
!	+10.0*(1.0/(float(abs(((size(impulse_input)/2)+1)-input_here_column))+1.0))

!reward function - approach 1:add to vein for those neurons that have data and the shift moves to centre
		
do here_column=1,size(impulse_action)
	vein_action(here_column)=vein_action(here_column)+0.001
	if (((size(impulse_input)/2)+1-input_here_column)*shift>0) then
		if (impulse_action(here_column)==1) then
			vein_action(here_column)=vein_action(here_column)+10.0			
		end if
	end if
end do


!increase vein weights accordingly
do here_column=1,size(impulse_action)
	!set vein change
	vein_change=vein_action(here_column)*0.1
	!add vein data to adjacent entries
	if (here_column>1) then
		vein_action(here_column)=vein_action(here_column)-vein_change
		vein_action(here_column-1)=vein_action(here_column-1)+vein_change
	end if
	if (here_column<size(impulse_action)) then
		vein_action(here_column)=vein_action(here_column)-vein_change
		vein_action(here_column+1)=vein_action(here_column+1)+vein_change
	end if	
	!move blood from the vein_action into the blood network	
	do there_column=(here_column-1)-1,(here_column-1)+1
		if ((there_column>=1) .and. (there_column<=maxim_column)) then
			blood(self_pos_blood(maxim_row,there_column,maxim_column),there_column,maxim_row)=&
				blood(self_pos_blood(maxim_row,there_column,maxim_column),there_column,maxim_row)+vein_change
			vein_action(here_column)=vein_action(here_column)-vein_change
		end if
	end do
end do



!use the vein data to change the brain weights for the connected brain neurons accordingly
do here_column=1,size(brain(1,:,1))
	do there_column=(here_column+1)-1,(here_column+1)+1
		brain(self_pos_brain(maxim_row+1,there_column-1,maxim_column),here_column,maxim_row)=&
			brain(self_pos_brain(maxim_row+1,there_column-1,maxim_column),here_column,maxim_row)+&
				int(vein_action(there_column)*(10**3)*scaling)
	end do
end do

!if requested a printout, print impulse_action and vein_action after the transitions are completed
if ((printed=='yes') .or. (printed=='debug')) then

	print'(A14,I0)',"impulse_input ",thrash
	do there_column=1,size(impulse_input)
		write(data_cha_impulse_input,"(I2)")impulse_input(there_column)
		print_row_impulse_input(there_column*2-1:there_column*2)=data_cha_impulse_input
	end do
	print *,print_row_impulse_input
	print*," "

	print'(A15,I0)',"impulse_action ",thrash
	do there_column=1,size(impulse_action)
		write(data_cha_impulse_action,"(I2)")impulse_action(there_column)
		print_row_impulse_action(there_column*2-1:there_column*2)=data_cha_impulse_action
	end do
	print *,print_row_impulse_action
	print*," "

	print'(A12,I0)',"vein_action ",thrash
	do there_column=1,size(vein_action)
		write(data_cha_vein_action,"(F7.3)")vein_action(there_column)
		print_row_vein_action(there_column*print_length_vein_action:there_column*print_length_vein_action+7)=data_cha_vein_action
	end do
	print *,print_row_vein_action
	print*," "

	print*,"Shift Value:",shift
	print*," "
	
end if

!remove impulse_action data
do here_column=1,size(impulse_action)
	impulse_action(here_column)=0
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
write(2,*) vein_action
write(2,*) impulse_action
write(2,*) impulse_input
write(2,*) thrash
write(2,*) active_data
write(2,*) grave
close(2)

!stop timer and print
call cpu_time(finish)
if ((printed=="yes") .or. (printed=='debug') .or. (printed=="network_only")) then
	call print_interval(start,finish)
	print*," "
end if
print*," "

!lag it if necessary
call sleep(lag)

end program power
