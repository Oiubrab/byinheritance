program network
use discrete_flesh
implicit none

!printing and variable input objects
integer :: cycles,maximum_columns,maximum_rows,active_data,grave=0
character(len=2) :: data_cha
character(len=3) :: data_cha_freeze
character(len=10000) :: valves,valve_value_cha,size_rows,size_columns,printed
character(len=10000) :: neuron_column_cha,neuron_row_cha,multiplier_scaling_cha,error_num
character(len=:),allocatable :: print_row,print_row_freeze

!incrementation and limiting objects
real :: multiplier_scaling
integer,parameter :: data_limiter=100
integer :: thrash,here_row,here_weight,there_row,there_column,here_column,h,k,g !g=multi_pos, h=conflict numerator
logical :: test_overlap_conflict, test_condition_conflict
integer,allocatable :: row_random(:),column_random(:)
integer,dimension(2) :: j_i

!network objects
integer, allocatable :: brain(:,:,:),brain_freeze(:,:) !brain_freeze stores a self pos value that gives the address that the data at position in the matrix corresponding to brain should go
real,allocatable :: blood(:,:,:)

!action objects
integer, allocatable :: impulse(:),impulse_input(:)
real, allocatable :: vein(:)

!debugging
integer :: counter
integer :: neuron_row,neuron_column

!time and chance
real :: fuck,start,finish

!boundary objects
integer :: valve_value
integer,dimension(4) :: boundaries !(bottom, left, right, top)

!for the time record          
call CPU_Time(start)

!make this a new run
call random_seed()

!prepare command line options
IF(COMMAND_ARGUMENT_COUNT().NE.8)THEN
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program valves valve_value maximum_columns maximum_rows neuron_column neuron_row multiplier_scaling printed'
	WRITE(*,*)'valves: left right up down custom'
	WRITE(*,*)'printed: yes no debug'
	STOP
ENDIF
!set the valves and cycles variables
CALL GET_COMMAND_ARGUMENT(1,valves)
CALL GET_COMMAND_ARGUMENT(2,valve_value_cha)
CALL GET_COMMAND_ARGUMENT(3,size_columns)
CALL GET_COMMAND_ARGUMENT(4,size_rows)
CALL GET_COMMAND_ARGUMENT(5,neuron_column_cha)
CALL GET_COMMAND_ARGUMENT(6,neuron_row_cha)
CALL GET_COMMAND_ARGUMENT(7,multiplier_scaling_cha)
CALL GET_COMMAND_ARGUMENT(8,printed)
READ(valve_value_cha,*)valve_value
READ(size_rows,*)maximum_rows
READ(size_columns,*)maximum_columns
READ(multiplier_scaling_cha,*)multiplier_scaling
READ(neuron_column_cha,*)neuron_column
READ(neuron_row_cha,*)neuron_row


if ((printed/='no') .and. (printed/='yes') .and. (printed/='debug')) then
	if ((valves/='up') .and. (valves/='down') .and. (valves/='left') .and. (valves/='right') .and. (valves/='custom')) then
		WRITE(*,*)'Execute program by format:'
		WRITE(*,*)'./program valves valve_value maximum_columns maximum_rows neuron_column neuron_row multiplier_scaling printed'
		WRITE(*,*)'valves: left right up down custom'
		WRITE(*,*)'printed: yes no debug'
		stop
	end if
end if

!set the arrays
allocate(blood(maximum_rows*maximum_columns,maximum_columns,maximum_rows))
allocate(brain((maximum_columns+2)*(maximum_rows+2),maximum_columns,maximum_rows))
allocate(brain_freeze(maximum_columns,maximum_rows))
allocate(vein(maximum_columns+2))
allocate(impulse(maximum_columns+2))
allocate(impulse_input(maximum_columns))
allocate(character(maximum_columns*2+1) :: print_row)
allocate(character(maximum_columns*3+2) :: print_row_freeze)
allocate(row_random(maximum_rows))
allocate(column_random(maximum_columns))

!retrieve previous network and move ahead thrash counter
open(unit=1,file="heartwork.txt")
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
read(1,*) impulse_input
read(1,*) thrash
read(1,*) active_data
read(1,*) grave
close(1)	

! boundaries variable records weights off of (bottom, left, right, top)
if (valves=="up") then
	boundaries=[valve_value,valve_value,valve_value,0]
else if (valves=="down") then
	boundaries=[0,valve_value,valve_value,valve_value]
else if (valves=="left") then
	boundaries=[valve_value,0,valve_value,valve_value]
else if (valves=="right") then
	boundaries=[valve_value,valve_value,0,valve_value]
else
	boundaries=[0,4,4,4]
end if


!let the extinction begin
!affect the brain with the blood multipliers
call infusion(brain,blood,multiplier_scaling)

!enact boundary conditions
call bondage(brain,boundaries)

!print probabilities for an individual neuron (debug)
if (printed=="debug") then
	print'(A37,I0,A9,I0)',"The probability enhancer for column ",neuron_column," and row ",neuron_row
	print*,"(column,row), is:"
	do here_weight=1,size(brain(:,1,1))
		if (brain(here_weight,neuron_column,neuron_row)>0) then
			j_i=point_pos_matrix(here_weight,maximum_columns)
			print'(A6,I2,A1,I2,A4,I0)',"For (",j_i(1),",",j_i(2),") p:",brain(here_weight,neuron_column,neuron_row)
		end if
	end do
	print*," "
end if

!count how much data is in brain
active_data=0
do there_row=1,size(brain(1,1,:))
	do there_column=1,size(brain(1,:,1))
		if (brain(self_pos(there_row,there_column,maximum_columns),there_column,there_row)==1) then
			active_data=active_data+1
		end if
	end do
end do

!all the transitions are first recorded in the brain_freeze matrix

!first, zero out brain_freeze
do here_row=1,size(brain(1,1,:))
	do here_column=1,size(brain(1,:,1))
		brain_freeze(here_column,here_row)=0
	end do
end do

!next, randomise the order of neuron operation. if not, data will bias the right side of the network
call randomised_list(row_random)
call randomised_list(column_random)
!then call in neuron_pre_fire to move all the data
do here_row=1,size(brain(1,1,:))
	do here_column=1,size(brain(1,:,1))

		!data is in the 3rd address, that corresponds to the position of the row/column, counting left to right, up to down, from the buffer
		if (brain(self_pos(row_random(here_row),column_random(here_column),maximum_columns),&
			column_random(here_column),row_random(here_row))==1) then
			
			j_i=[column_random(here_column),row_random(here_row)]
			call neuron_pre_fire(brain,brain_freeze,j_i)
			
		end if

	end do
end do

!then conflicts are checked for
!for each position here_column,here_row, conflicts are tested throughout the matrix at there_column,there_row
test_overlap_conflict=.true.
test_condition_conflict=.true.
counter=0
do while ((test_overlap_conflict .eqv. .true.) .or. (test_condition_conflict .eqv. .true.))

	test_condition_conflict=.false.
	test_overlap_conflict=.false.
	
	!this whole do loop checks at each brain position, each other position
	do here_row=1,size(brain_freeze(1,:))
		do here_column=1,size(brain_freeze(:,1))

			!ensure each neuron that is coming in has space
			!this is for test1 of the power script
			do there_column=1,size(brain(1,:,1))
				if (brain_freeze(here_column,here_row)==self_pos(1,there_column,maximum_columns)) then
					test_condition_conflict=.true.
					j_i=[here_column,here_row]
					call neuron_pre_fire(brain,brain_freeze,j_i)
				end if
			end do

			!only check non-zero entries
			if (brain_freeze(here_column,here_row)/=0) then
				
				!before 20 tries, the system is considered to work fine
				if (counter<=20) then
				
					do there_row=1,size(brain_freeze(1,:))
						do there_column=1,size(brain_freeze(:,1))

							!store all the target values in the multi_target array
							if ((here_column/=there_column) .or. (here_row/=there_row)) then
								if (brain_freeze(here_column,here_row)==brain_freeze(there_column,there_row)) then
									
									!The neuron with the biggest weight gets first dibs
									if (brain(brain_freeze(here_column,here_row),there_column,there_row)<brain(brain_freeze(there_column,there_row),&
										here_column,here_row)) then
										
										j_i=[there_column,there_row]
										
									else if (brain(brain_freeze(here_column,here_row),there_column,there_row)>&
										brain(brain_freeze(there_column,there_row),here_column,here_row)) then
										
										j_i=[here_column,here_row]
										
									else
										!if weights are equal, randomise selection
										call random_number(fuck)
										if (fuck>0.5) then
											j_i=[there_column,there_row]
										else
											j_i=[here_column,here_row]
										end if
										
									end if
									
									call neuron_pre_fire(brain,brain_freeze,j_i,brain_freeze(here_column,here_row))
									test_overlap_conflict=.true.
									
								end if

							end if

						end do
					end do

				!after 100 tries, clearly something is stuck, so just skip problem neurons
				else
					
					do there_row=1,size(brain_freeze(1,:))
						do there_column=1,size(brain_freeze(:,1))
						
							if ((here_column/=there_column) .or. (here_row/=there_row)) then
							
								!stop neurons moving data if they are both going to the same destination
								if (brain_freeze(here_column,here_row)==brain_freeze(there_column,there_row)) then											
									
									brain_freeze(here_column,here_row)=0
									brain_freeze(there_column,there_row)=0
									test_overlap_conflict=.true.
								
								!stop neuron here_column,here_row moving data if it's destination is both full and not moving 
								else if ((brain_freeze(here_column,here_row)==self_pos(there_row,there_column,maximum_columns)) .and. &
									(brain_freeze(there_column,there_row)==0) .and. &
									brain(brain_freeze(here_column,here_row),there_column,there_row)==1) then
									
									brain_freeze(here_column,here_row)=0
									test_overlap_conflict=.true.
									
								end if
								
							end if
								
						end do
					end do
				
				end if
			end if
			
		end do
	end do

	!debuggling: save and print tool
	if ((counter>20) .and. (test_overlap_conflict .eqv. .false.)) then
		write(error_num,"(I0)")thrash
		open(unit=2,file="error_folder/neurotic_error_"//trim(error_num)//".txt")
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
		write(2,*) impulse_input
		write(2,*) thrash
		write(2,*) active_data
		write(2,*) grave
		close(2)
		
	end if

	counter=counter+1
	
end do

!debuggling: print brain_freeze 
if (printed=='debug') then
	print*,"Brain Freeze After",thrash
	do there_row=1,size(brain_freeze(1,:))
		do there_column=1,size(brain_freeze(:,1))
			write(data_cha_freeze,"(I3)")brain_freeze(there_column,there_row)
			print_row_freeze(there_column*3-2:there_column*3)=data_cha_freeze
		end do
		print *,print_row_freeze
	end do
	print*," "
end if

!finally, transact the recorded transitions in brain_freeze
call reflect(impulse,brain,brain_freeze,grave)

!steadily detract brain probability weights
do there_row=1,size(brain(1,1,:))
	do there_column=1,size(brain(1,:,1))
		do h=1,size(brain(:,1,1))
			if ((brain(h,there_column,there_row)>0) .and. (self_pos(there_row,there_column,maximum_columns)/=h)) then
				brain(h,there_column,there_row)=brain(h,there_column,there_row)-(1+((brain(h,there_column,there_row))/data_limiter))
			end if
		end do
	end do
end do	

!count how much data is in brain
active_data=0
do there_row=1,size(brain(1,1,:))
	do there_column=1,size(brain(1,:,1))
		if (brain(self_pos(there_row,there_column,maximum_columns),there_column,there_row)==1) then
			active_data=active_data+1
		end if
	end do
end do

!if requested a printout, print brain after the transitions are completed
if ((printed=='yes') .or. (printed=='debug')) then
	print'(A6,I0,A14,I0,A12,I0)',"Brain ",thrash," active data: ",active_data," dead data: ",grave
	do there_row=1,size(brain(1,1,:))
		do there_column=1,size(brain(1,:,1))
			write(data_cha,"(I2)")brain(self_pos(there_row,there_column,maximum_columns),there_column,there_row)
			print_row(there_column*2-1:there_column*2)=data_cha
		end do
		print *,print_row
	end do
	!print*," "
end if

!debuggling: check if any neurons are greater than 1
do here_row=1,size(brain(1,1,:))
	do here_column=1,size(brain(1,:,1))
		if ((brain(self_pos(here_row,here_column,maximum_columns),here_column,here_row)>1) .or. &
			(brain(self_pos(here_row,here_column,maximum_columns),here_column,here_row)<0)) then
			
			stop
			
		end if
	end do
end do

!write the networks to file
open(unit=2,file="neurotic.txt")
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
write(2,*) impulse_input
write(2,*) thrash
write(2,*) active_data
write(2,*) grave
close(2)

call CPU_Time(finish)
if ((printed=="yes") .or. (printed=='debug')) then
	call print_interval(start,finish)
	print*," "
end if

end program
