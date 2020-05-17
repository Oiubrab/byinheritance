program network
use discrete_flesh
use cudafor
implicit none

integer :: cycles,maximum_columns,maximum_rows,lag,active_data,grave=0
character(len=2) :: data_cha
character(len=10000) :: valves,cycled,size_rows,size_columns,lag_cha,printed,neuron_column_cha,neuron_row_cha
character(len=:),allocatable :: print_row

integer :: thrash,i,l,m,j,h,k,g,test_overlap_conflict,test_outside_conflict !i,l=rows, j,m=columns, g=multi_pos, h=conflict numerator, test_overlap_conflict=true/false conflicts found
integer :: neuron_row,neuron_column
integer,dimension(2) :: j_i

integer, allocatable :: brain(:,:,:),brain_freeze(:,:) !brain_freeze stores a self pos value that gives the address that the data at position in the matrix corresponding to brain should go
integer,dimension(9) :: multi_target

real :: fuck,start,finish

integer,dimension(4) :: boundaries !(bottom, left, right, top)

!for the time record          
call CPU_Time(start)

!make this a new run
call random_seed()

!prepare command line options
IF(COMMAND_ARGUMENT_COUNT().NE.8)THEN
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program valves cycles maximum_columns maximum_rows neuron_column neuron_row lag printed'
	WRITE(*,*)'valves: closed left_open'
	WRITE(*,*)'printed: yes no debug'
	STOP
ENDIF
!set the valves and cycles variables
CALL GET_COMMAND_ARGUMENT(1,valves)
CALL GET_COMMAND_ARGUMENT(2,cycled)
CALL GET_COMMAND_ARGUMENT(3,size_columns)
CALL GET_COMMAND_ARGUMENT(4,size_rows)
CALL GET_COMMAND_ARGUMENT(5,neuron_column_cha)
CALL GET_COMMAND_ARGUMENT(6,neuron_row_cha)
CALL GET_COMMAND_ARGUMENT(7,lag_cha)
CALL GET_COMMAND_ARGUMENT(8,printed)
READ(size_rows,*)maximum_rows
READ(size_columns,*)maximum_columns
READ(neuron_column_cha,*)neuron_column
READ(neuron_row_cha,*)neuron_row
READ(cycled,*)cycles
READ(lag_cha,*)lag

if ((printed/='no') .and. (printed/='yes') .and. (printed/='debug')) then
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program valves cycles maximum_columns maximum_rows neuron_column neuron_row lag printed'
	WRITE(*,*)'valves: closed left_open'
	WRITE(*,*)'printed: yes no debug'
	stop
end if

!print*,maximum_rows,maximum_columns
!set the arrays
allocate(brain(1:maximum_columns*maximum_rows+2*(maximum_columns+maximum_rows)-4,1:maximum_columns,1:maximum_rows))
allocate(brain_freeze(1:maximum_columns,1:maximum_rows))
allocate(character(maximum_columns*2+1) :: print_row)
!print*,size(brain(1,:,1)),size(brain(1,:,1))
!print*,len(print_row)

!expand maximum_rows and maximum_columns to include buffer zone
maximum_columns=maximum_columns
maximum_rows=maximum_rows

!make sure the matrix starts off with zeros
do i=1,size(brain(1,1,:))
	do j=1,size(brain(1,:,1))
		brain_freeze(j,i)=0
		do k=1,size(brain(:,1,1))
			brain(k,j,i)=0
		end do
	end do
end do

! boundaries variable records weights off of (bottom, left, right, top)
boundaries=[0,1000,1000,1000]

do thrash=0,cycles-1
	call sleep(lag)
	
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
	
	!enact boundary conditions
	call bondage(brain,boundaries)
	
	!print probabilities for an individual neuron
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

	!print the matrix before it gets operated on
	if ((printed=='yes') .or. (printed=='debug')) then
		print'(A13,I0,A14,I0,A12,I0)',"Brain Before ",thrash+1," active data: ",active_data," dead data: ",grave
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
	do while ((test_overlap_conflict==1) .or. (test_outside_conflict==1))

		test_overlap_conflict=0
		test_outside_conflict=0

		!print*,"hello from the gutter",test_overlap_conflict




		
		!this whole do loop checks at each brain position, each other position
		do i=1,size(brain_freeze(1,:))
			do j=1,size(brain_freeze(:,1))

				!print*,"roger explosion",test_overlap_conflict

				!check if neuron target is outside the matrix
				if ((valves=="closed") .and. (brain_freeze(j,i)/=0)) then
					do while ((brain_freeze(j,i)<maximum_columns+4) .or. &
						(brain_freeze(j,i)>(maximum_rows+2)*(maximum_columns+2)-(maximum_columns+1))&
						.or. (mod(brain_freeze(j,i),(maximum_columns+2))==1) .or. &
						(mod(brain_freeze(j,i),(maximum_columns+2))==0))	

						test_outside_conflict=1
						j_i=[j,i]
						!print*,maximum_columns+4,(maximum_rows+2)*(maximum_columns+2)-(maximum_columns+2)-2,&
						!	mod(brain_freeze(j,i),(maximum_columns+2)),mod(brain_freeze(j,i),(maximum_columns+2))
						!print*,brain_freeze(j,i),j,i
						call neuron_pre_fire(brain,brain_freeze,j_i)
						!print*,brain_freeze(j,i),j,i
					end do
				end if

				!only check non-zero entries
				if (brain_freeze(j,i)/=0) then
					multi_target=(/0,0,0,0,0,0,0,0,0/)
					!print*,multi_target

					do l=1,size(brain_freeze(1,:))
						do m=1,size(brain_freeze(:,1))
							
							!print*,"fuck you",test_overlap_conflict
							!print*,j,m,i,l,brain_freeze(j,i),brain_freeze(m,l),multi_target(1)

							!store all the target values in the multi_target array
							if ((j/=m) .or. (i/=l)) then
								if ((brain_freeze(j,i)==brain_freeze(m,l)) .and. (multi_target(1)==0)) then
									multi_target(1)=brain_freeze(j,i)
									!positions of the offending double data darsterdly duo deliniated in multi_target
									multi_target(2)=self_pos(i,j,maximum_columns)
									multi_target(3)=self_pos(l,m,maximum_columns)
									test_overlap_conflict=1
									!g holds the place before the first 0 entry in multi_target
									g=3

									!print*,multi_target

								else if (brain_freeze(j,i)==brain_freeze(m,l)) then
									!g is moved to the first 0 entry in multi_target
									do while (multi_target(g)>0)
										g=g+1
									end do

									multi_target(g)=self_pos(l,m,maximum_columns)

									!print*,multi_target

								end if
								!the last g is the address of the last address of origin in multi_target
							end if

							!print*,"and then some",test_overlap_conflict

						end do
					end do

				end if



				!print*,"balls to the wall",test_overlap_conflict



				if (test_overlap_conflict==1) then
					!find the lucky neuron that gets to blow it's load
					!initialise h so that the first do loop can randomise h
					h=0
					do while ((h==0) .or. (h==1))
						call RANDOM_NUMBER(fuck)
						h=fuck*(g+1)
					end do

					!print*,"fuck like an animal",test_overlap_conflict
					!the unlucky neurons now must find another target
					
					g=2

					do while (multi_target(g)>0)
						if (g/=h) then

							!print*,"I fuck like a beast",test_overlap_conflict

							j_i=point_pos_matrix(multi_target(g),maximum_columns)
							call neuron_pre_fire(brain,brain_freeze,j_i)

						end if
						g=g+1
					end do

				end if



			end do
		end do





	end do

	!debuggling: print brain_freeze 
	if (printed=='debug') then
		print*,"Brain Freeze After",thrash+1
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
		print'(A12,I0,A14,I0,A12,I0)',"Brain After ",thrash+1," active data: ",active_data," dead data: ",grave
		do l=1,size(brain(1,1,:))
			do m=1,size(brain(1,:,1))
				write(data_cha,"(I2)")brain(self_pos(l,m,maximum_columns),m,l)
				print_row(m*2-1:m*2)=data_cha
			end do
			print *,print_row
		end do
		print*," "
	end if

end do

call CPU_Time(finish)
call print_interval(start,finish)
end program
