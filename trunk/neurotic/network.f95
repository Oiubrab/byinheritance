program network
use discrete_flesh
use cudafor
implicit none

!printing objects
integer :: cycles,maximum_columns,maximum_rows,lag,active_data,grave=0
character(len=2) :: data_cha
character(len=10000) :: valves,cycled,size_rows,size_columns,lag_cha,printed,neuron_column_cha,neuron_row_cha,multiplier_scaling_cha
character(len=:),allocatable :: print_row

!incrementation and limiting objects
real :: multiplier_scaling
integer :: thrash,i,l,m,j,h,k,g,test_overlap_conflict,test_outside_conflict,test_condition_conflict !i,l=rows, j,m=columns, g=multi_pos, h=conflict numerator, test_overlap_conflict=true/false conflicts found
integer :: neuron_row,neuron_column
integer,dimension(2) :: j_i

!network objects
integer, allocatable :: brain(:,:,:),brain_freeze(:,:) !brain_freeze stores a self pos value that gives the address that the data at position in the matrix corresponding to brain should go
real,allocatable :: blood(:,:,:)

!time and chance
real :: fuck,start,finish

integer,dimension(4) :: boundaries !(bottom, left, right, top)

!for the time record          
call CPU_Time(start)

!make this a new run
call random_seed()

!prepare command line options
IF(COMMAND_ARGUMENT_COUNT().NE.8)THEN
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program valves maximum_columns maximum_rows neuron_column neuron_row lag multiplier_scaling printed'
	WRITE(*,*)'valves: closed open bottom_open'
	WRITE(*,*)'printed: yes no debug'
	STOP
ENDIF
!set the valves and cycles variables
CALL GET_COMMAND_ARGUMENT(1,valves)
CALL GET_COMMAND_ARGUMENT(2,size_columns)
CALL GET_COMMAND_ARGUMENT(3,size_rows)
CALL GET_COMMAND_ARGUMENT(4,neuron_column_cha)
CALL GET_COMMAND_ARGUMENT(5,neuron_row_cha)
CALL GET_COMMAND_ARGUMENT(6,lag_cha)
CALL GET_COMMAND_ARGUMENT(7,multiplier_scaling_cha)
CALL GET_COMMAND_ARGUMENT(8,printed)
READ(size_rows,*)maximum_rows
READ(size_columns,*)maximum_columns
READ(multiplier_scaling_cha,*)multiplier_scaling
READ(neuron_column_cha,*)neuron_column
READ(neuron_row_cha,*)neuron_row
READ(lag_cha,*)lag

if ((printed/='no') .and. (printed/='yes') .and. (printed/='debug')) then
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program valves maximum_columns maximum_rows neuron_column neuron_row lag multiplier_scaling printed'
	WRITE(*,*)'valves: closed open bottom_open'
	WRITE(*,*)'printed: yes no debug'
	stop
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
boundaries=[0,10,10,10]


!let the extinction begin
!affect the brain with the blood multipliers
call infusion(brain,blood,multiplier_scaling)


!test:inject ones into matrix
!here this is done in a sweeping pattern, from left to right, then back to left

!if ((-1)**((thrash/size(brain(1,:,1)))+1)==-1) then
!	!add data to column
!	brain(mod(thrash,maximum_columns)+4+maximum_columns,mod(thrash,size(brain(1,:,1)))+1,1)=1 !move from left to right
!else
!	!add data to column
!	brain((maximum_columns*2+3)-mod(thrash,maximum_columns),&
!		size(brain(1,:,1))-mod(thrash,size(brain(1,:,1))),1)=1 !move from right to left
!end if	

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
test_outside_conflict=1
test_condition_conflict=1
do while ((test_overlap_conflict==1) .or. (test_outside_conflict==1)  .or. (test_condition_conflict==1))

	test_condition_conflict=0
	test_overlap_conflict=0
	test_outside_conflict=0
	
	!this whole do loop checks at each brain position, each other position
	do i=1,size(brain_freeze(1,:))
		do j=1,size(brain_freeze(:,1))

			!switch closing the network, keeping all the data inside
			if ((valves=="closed") .and. (brain_freeze(j,i)/=0)) then
				if ((brain_freeze(j,i)<maximum_columns+4) .or. &
					(brain_freeze(j,i)>(maximum_rows+2)*(maximum_columns+2)-(maximum_columns+1))&
					.or. (mod(brain_freeze(j,i),(maximum_columns+2))==1) .or. &
					(mod(brain_freeze(j,i),(maximum_columns+2))==0)) then	

					test_outside_conflict=1
					j_i=[j,i]

					call neuron_pre_fire(brain,brain_freeze,j_i)

				end if
			end if
			
			!switch closing the network, except for the bottom interface
			if ((valves=="bottom_open") .and. (brain_freeze(j,i)/=0)) then
				if ((brain_freeze(j,i)<maximum_columns+4) .or. &
					(mod(brain_freeze(j,i),(maximum_columns+2))==1) .or. &
					(mod(brain_freeze(j,i),(maximum_columns+2))==0)) then

					test_outside_conflict=1
					j_i=[j,i]

					call neuron_pre_fire(brain,brain_freeze,j_i)

				end if
			end if

			!ensure each neuron that is coming in has space (being built)
			if (brain_freeze(j,i)==self_pos(1,maximum_columns/2,maximum_columns)) then
				test_condition_conflict=1
				j_i=[j,i]
				call neuron_pre_fire(brain,brain_freeze,j_i)
			end if

			!only check non-zero entries
			if (brain_freeze(j,i)/=0) then

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
									
								call neuron_pre_fire(brain,brain_freeze,j_i)
								test_overlap_conflict=1
								
							end if

						end if

					end do
				end do

			end if
			
		end do
	end do
	
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
