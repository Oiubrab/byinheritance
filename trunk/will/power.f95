program power
use flesh_out
implicit none

!network read/write
integer, allocatable :: brain(:,:,:)
real,allocatable :: blood(:,:,:)
integer :: thrash,active_data,grave
character(len=1000) :: maxim_column_cha,maxim_row_cha,network_scaling_cha,printed

!printing
character(len=:),allocatable :: print_row_impulse
character(len=2) :: data_cha_impulse
integer :: print_length_vein=7
character(len=print_length_vein) :: data_cha_vein
character(len=:),allocatable :: print_row_vein

!action objects
integer, allocatable :: impulse(:)
real, allocatable :: vein(:)
real :: transition

!timing control
character(len=1000) :: lag_cha
integer :: lag

!network dimension
integer :: maxim_row,maxim_column,here_row,here_column,ages,special,there_column

!activation options
real :: scaling
logical :: switch1=.true.
logical :: switch2=.true.

!prepare command line options
IF(COMMAND_ARGUMENT_COUNT().NE.5)THEN
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program maximum_columns maximum_rows lag network_scaling printed'
	WRITE(*,*)'printed: yes no debug'
	STOP
ENDIF

!set the column/row variables
CALL GET_COMMAND_ARGUMENT(1,maxim_column_cha)
CALL GET_COMMAND_ARGUMENT(2,maxim_row_cha)
CALL GET_COMMAND_ARGUMENT(3,lag_cha)
CALL GET_COMMAND_ARGUMENT(4,network_scaling_cha)
CALL GET_COMMAND_ARGUMENT(5,printed)
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
allocate(vein(maxim_column+2))
allocate(impulse(maxim_column+2))
allocate(character((maxim_column+2)*2+1) :: print_row_impulse)
allocate(character((maxim_column+2)*print_length_vein+7) :: print_row_vein)

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
read(1,*) vein
read(1,*) impulse
read(1,*) thrash
read(1,*) active_data
read(1,*) grave
close(1)	

!test brain injection
if (brain(self_pos_brain(1,maxim_column/2,maxim_column),maxim_column/2,1)==0) then
	brain(self_pos_brain(1,maxim_column/2,maxim_column),maxim_column/2,1)=1
end if

if (switch1==.true.) then

	!add blood to the vein neuron if there is data in the impulse neuron
	do here_column=1,size(impulse)
		if (impulse(here_column)==1) then
			vein(here_column)=vein(here_column)+5.0
			!add the blood, depending on the desired reward
			if ((here_column==maxim_column/2) .or. (here_column==(maxim_column/2)-1) .or. (here_column==(maxim_column/2)+1)) then
				vein(here_column)=vein(here_column)+5.0
			end if
		else
			vein(here_column)=vein(here_column)+1.0
		end if
		!increase brain and blood weights accordingly
		do there_column=(here_column-1)-1,(here_column-1)+1
			if ((there_column>=1) .and. (there_column<=maxim_column)) then
				!move blood from the vein into the blood network
				blood(self_pos_blood(maxim_row,there_column,maxim_column),there_column,maxim_row)=&
					blood(self_pos_blood(maxim_row,there_column,maxim_column),there_column,maxim_row)+vein(here_column)*0.05
				vein(here_column)=vein(here_column)-vein(here_column)*0.05
			end if
		end do
	end do

end if



!change the brain weights for the connected brain neurons accordingly
do here_column=1,size(brain(1,:,1))
	do there_column=(here_column+1)-1,(here_column+1)+1
		brain(self_pos_brain(maxim_row+1,there_column-1,maxim_column),here_column,maxim_row)=&
			brain(self_pos_brain(maxim_row+1,there_column-1,maxim_column),here_column,maxim_row)+int(vein(there_column)*(10**3)*scaling)
	end do
end do

!vein(1)=100.;vein(12)=100.0

!do stuff with impulse activation

!if requested a printout, print impulse and vein after the transitions are completed
if ((printed=='yes') .or. (printed=='debug')) then

	print'(A8,I0)',"Impulse ",thrash
	do there_column=1,size(impulse)
		write(data_cha_impulse,"(I2)")impulse(there_column)
		print_row_impulse(there_column*2-1:there_column*2)=data_cha_impulse
	end do
	print *,print_row_impulse
	print*," "

	print'(A5,I0)',"Vein ",thrash
	do there_column=1,size(vein)
		write(data_cha_vein,"(F7.3)")vein(there_column)
		print_row_vein(there_column*print_length_vein:there_column*print_length_vein+7)=data_cha_vein
	end do
	print *,print_row_vein
	print*," "
	
end if

!remove impulse data
if (switch2==.true.) then
	do here_column=1,size(impulse)
		impulse(here_column)=0
	end do	
end if
	

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
write(2,*) vein
write(2,*) impulse
write(2,*) thrash
write(2,*) active_data
write(2,*) grave
close(2)

!lag it if necessary
call sleep(lag)

end program power
