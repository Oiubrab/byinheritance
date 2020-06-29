program network
use discrete_flesh
use cudafor
implicit none

!printing objects
integer :: cycles,maximum_columns,maximum_rows,lag,active_data,grave=0
character(len=2) :: data_cha
character(len=10000) :: valves,valve_value_cha,size_rows,size_columns,lag_cha,printed,neuron_column_cha,neuron_row_cha,multiplier_scaling_cha,error_num
character(len=:),allocatable :: print_row

!incrementation and limiting objects
real :: multiplier_scaling
integer :: thrash,i,l,m,j,h,k,g,test_overlap_conflict,test_condition_conflict !i,l=rows, j,m=columns, g=multi_pos, h=conflict numerator, test_overlap_conflict=true/false conflicts found
integer :: neuron_row,neuron_column
integer,dimension(2) :: j_i

!network objects
integer, allocatable :: brain(:,:,:),brain_freeze(:,:) !brain_freeze stores a self pos value that gives the address that the data at position in the matrix corresponding to brain should go
real,allocatable :: blood(:,:,:)

!debugging
integer :: counter

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
IF(COMMAND_ARGUMENT_COUNT().NE.9)THEN
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program valves maximum_columns maximum_rows neuron_column neuron_row lag multiplier_scaling printed'
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
CALL GET_COMMAND_ARGUMENT(7,lag_cha)
CALL GET_COMMAND_ARGUMENT(8,multiplier_scaling_cha)
CALL GET_COMMAND_ARGUMENT(9,printed)
READ(valve_value_cha,*)valve_value
READ(size_rows,*)maximum_rows
READ(size_columns,*)maximum_columns
READ(multiplier_scaling_cha,*)multiplier_scaling
READ(neuron_column_cha,*)neuron_column
READ(neuron_row_cha,*)neuron_row
READ(lag_cha,*)lag

if ((printed/='no') .and. (printed/='yes') .and. (printed/='debug')) then
	if ((valves/='up') .and. (valves/='down') .and. (valves/='left') .and. (valves/='right') .and. (valves/='custom')) then
		WRITE(*,*)'Execute program by format:'
		WRITE(*,*)'./program valves valve_value maximum_columns maximum_rows neuron_column neuron_row lag multiplier_scaling printed'
		WRITE(*,*)'valves: left right up down custom'
		WRITE(*,*)'printed: yes no debug'
		stop
	end if
end if

!print*,maximum_rows,maximum_columns
!set the arrays
allocate(blood(maximum_rows*maximum_columns,maximum_columns,maximum_rows))
allocate(brain(1:maximum_columns*maximum_rows+2*(maximum_columns+maximum_rows)-4,1:maximum_columns,1:maximum_rows))
allocate(brain_freeze(1:maximum_columns,1:maximum_rows))
allocate(character(maximum_columns*2+1) :: print_row)
!print*,size(brain(1,:,1)),size(brain(1,:,1))
!print*,len(print_row)

!retrieve previous network and move ahead thrash counter
open(unit=1,file="heartwork.txt")
do i=1,size(blood(1,1,:))
	do j=1,size(blood(1,:,1))
		read(1,*) blood(:,j,i)
	end do
end do
do i=1,size(brain(1,1,:))
	do j=1,size(brain(1,:,1))
		read(1,*) brain(:,j,i)
	end do
end do
read(1,*) thrash
read(1,*) active_data
read(1,*) grave
close(1)	

! boundaries variable records weights off of (bottom, left, right, top)
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


if (brain(self_pos(1,maximum_columns/2,maximum_columns),maximum_columns/2,1)==0) then
	brain(self_pos(1,maximum_columns/2,maximum_columns),maximum_columns/2,1)=1
end if

!enact boundary conditions
call bondage(brain,boundaries)

!print probabilities for an individual neuron (debug)
if (printed=="debug") then
	print'(A37,I0,A9,I0)',"The probability enhancer for column ",neuron_column," and row ",neuron_row
	print*,"(column,row), is:"
	do i=1,size(brain(:,1,1))
		if (brain(i,neuron_column,neuron_row)>0) then
			j_i=point_pos_matrix(i,maximum_columns)
			print'(A6,I2,A1,I2,A4,I0)',"For (",j_i(1),",",j_i(2),") p:",brain(i,neuron_column,neuron_row)
		end if
	end do
	print*," "
end if

!count how much data is in brain
active_data=0
do l=1,size(brain(1,1,:))
	do m=1,size(brain(1,:,1))
		if (brain(self_pos(l,m,maximum_columns),m,l)==1) then
			active_data=active_data+1
		end if
	end do
end do

!print the matrix before it gets operated on (debug)
if (printed=='debug') then
	print'(A13,I0,A14,I0,A12,I0)',"Brain Before ",thrash," active data: ",active_data," dead data: ",grave
	do l=1,size(brain(1,1,:))
		do m=1,size(brain(1,:,1))

			write(data_cha,"(I2)")brain(self_pos(l,m,maximum_columns),m,l)
			print_row(m*2-1:m*2)=data_cha
			
		end do
		print *,print_row

	end do
	print*," "
end if

!all the transitions are first recorded in the brain_freeze matrix

!first, zero out brain_freeze
do i=1,size(brain(1,1,:))
	do j=1,size(brain(1,:,1))
		brain_freeze(j,i)=0
	end do
end do

!then call in neuron_pre_fire to move all the data
do i=1,size(brain(1,1,:))
	do j=1,size(brain(1,:,1))

		!data is in the 3rd address, that corresponds to the position of the row/column, counting left to right, up to down, from the buffer
		if (brain(self_pos(i,j,maximum_columns),j,i)==1) then
			j_i=[j,i]
			call neuron_pre_fire(brain,brain_freeze,j_i)
		end if

	end do
end do

!then conflicts are checked for
!for each position j,i, conflicts are tested throughout the matrix at m,l
test_overlap_conflict=1
test_condition_conflict=1
counter=0
do while ((test_overlap_conflict==1) .or. (test_condition_conflict==1))

	test_condition_conflict=0
	test_overlap_conflict=0
	
	!this whole do loop checks at each brain position, each other position
	do i=1,size(brain_freeze(1,:))
		do j=1,size(brain_freeze(:,1))

			!ensure each neuron that is coming in has space
			!this is for the test injector
			if (brain_freeze(j,i)==self_pos(1,maximum_columns/2,maximum_columns)) then
				test_condition_conflict=1
				j_i=[j,i]
				call neuron_pre_fire(brain,brain_freeze,j_i)
			end if

			!only check non-zero entries
			if (brain_freeze(j,i)/=0) then
				
				!before 300 tries, the system is considered to work fine
				if (counter<=300) then
				
					do l=1,size(brain_freeze(1,:))
						do m=1,size(brain_freeze(:,1))

							!store all the target values in the multi_target array
							if ((j/=m) .or. (i/=l)) then
								if (brain_freeze(j,i)==brain_freeze(m,l)) then
									
									!The neuron with the biggest weight gets first dibs
									if (brain(brain_freeze(j,i),m,l)<brain(brain_freeze(m,l),j,i)) then
										j_i=[m,l]
									else if (brain(brain_freeze(j,i),m,l)>brain(brain_freeze(m,l),j,i)) then
										j_i=[j,i]
									else
										!if weights are equal, randomise selection
										call random_number(fuck)
										if (fuck>0.5) then
											j_i=[m,l]
										else
											j_i=[j,i]
										end if
										
									end if
									
									call neuron_pre_fire(brain,brain_freeze,j_i,brain_freeze(j,i))
									test_overlap_conflict=1
									
								end if

							end if

						end do
					end do

				!after 300 tries, clearly something is stuck, so just skip problem neurons
				else
					
					do l=1,size(brain_freeze(1,:))
						do m=1,size(brain_freeze(:,1))
						
							if ((j/=m) .or. (i/=l)) then
							
								!stop neurons moving data if they are both going to the same destination
								if (brain_freeze(j,i)==brain_freeze(m,l)) then											
									
									brain_freeze(j,i)=0
									brain_freeze(m,l)=0
									test_overlap_conflict=1
								
								!stop neuron j,i moving data if it's destination is both full and not moving 
								else if ((brain_freeze(j,i)==self_pos(l,m,maximum_columns)) .and. (brain_freeze(m,l)==0) .and. &
									brain(brain_freeze(j,i),m,l)==1) then
									
									brain_freeze(j,i)=0
									test_overlap_conflict=1
									
								end if
								
							end if
								
						end do
					end do
				
				end if
			end if
			
		end do
	end do

	!debuggling: save and print tool
	if ((counter>300) .and. (test_overlap_conflict==0)) then
		write(error_num,"(I0)")thrash
		open(unit=2,file="error_folder/neurotic_error_"//trim(error_num)//".txt")
		do i=1,size(blood(1,1,:))
			do j=1,size(blood(1,:,1))
				write(2,*) blood(:,j,i)
			end do
		end do
		do i=1,size(brain(1,1,:))
			do j=1,size(brain(1,:,1))
				write(2,*) brain(:,j,i)
			end do
		end do
		write(2,*) thrash
		write(2,*) active_data
		write(2,*) grave
		close(2)
		
		!test_condition_conflict=0
		!test_overlap_conflict=0
		!stop
		
	end if

	counter=counter+1
	
end do

brain(self_pos(1,maximum_columns/2,maximum_columns),maximum_columns/2,1)=1

!debuggling: print brain_freeze 
if (printed=='debug') then
	print*,"Brain Freeze After",thrash
	do l=1,size(brain_freeze(1,:))
		do m=1,size(brain_freeze(:,1))
			write(data_cha,"(I2)")brain_freeze(m,l)
			print_row(m*2-1:m*2)=data_cha
		end do
		print *,print_row
	end do
	print*," "
end if

!finally, transact the recorded transitions in brain_freeze
call reflect(brain,brain_freeze,valves,grave)

!steadily detract brain probability weights
do l=1,size(brain(1,1,:))
	do m=1,size(brain(1,:,1))
		do h=1,size(brain(:,1,1))
			if ((brain(h,m,l)>0) .and. (self_pos(l,m,maximum_columns)/=h)) then
				brain(h,m,l)=brain(h,m,l)-1
			end if
		end do
	end do
end do	

!count how much data is in brain
active_data=0
do l=1,size(brain(1,1,:))
	do m=1,size(brain(1,:,1))
		if (brain(self_pos(l,m,maximum_columns),m,l)==1) then
			active_data=active_data+1
		end if
	end do
end do

!if requested a printout, print brain after the transitions are completed
if ((printed=='yes') .or. (printed=='debug')) then
	print'(A6,I0,A14,I0,A12,I0)',"Brain ",thrash," active data: ",active_data," dead data: ",grave
	do l=1,size(brain(1,1,:))
		do m=1,size(brain(1,:,1))
			write(data_cha,"(I2)")brain(self_pos(l,m,maximum_columns),m,l)
			print_row(m*2-1:m*2)=data_cha
		end do
		print *,print_row
	end do
	print*," "
end if

!debuggling: check if any neurons are greater than 1
do i=1,size(brain(1,1,:))
	do j=1,size(brain(1,:,1))
		if ((brain(self_pos(i,j,maximum_columns),j,i)>1) .or. (brain(self_pos(i,j,maximum_columns),j,i)<0)) then
			stop
		end if
	end do
end do

!write the networks to file
open(unit=2,file="neurotic.txt")
do i=1,size(blood(1,1,:))
	do j=1,size(blood(1,:,1))
		write(2,*) blood(:,j,i)
	end do
end do
do i=1,size(brain(1,1,:))
	do j=1,size(brain(1,:,1))
		write(2,*) brain(:,j,i)
	end do
end do
write(2,*) thrash
write(2,*) active_data
write(2,*) grave
close(2)

call CPU_Time(finish)
if ((printed=="yes") .or. (printed=='debug')) then
	call print_interval(start,finish)
end if

!lag it if necessary
call sleep(lag)

end program
