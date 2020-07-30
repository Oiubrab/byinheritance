module answers
use emerge
use flesh_out
use flesh
use discrete_flesh
implicit none
contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Questions ancilliary subprograms!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Each function or subroutine on column rung relies on functions or subroutines on the rung before it. !!
!! If column function/subroutine cannot function without the subroutines/functions on its rung          !!
!! (explicitly), or on rungs above it, then it should be moved up column rung.                          !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!        rung one          !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




!read in the network or write out to a text file
subroutine read_write(brain,blood,vein,impulse,impulse_input,epoch,active_data,grave,direction)
	integer,dimension(*),intent(inout) :: brain(:,:,:)
	real,dimension(*),intent(inout) :: blood(:,:,:)
	integer, dimension(*) :: impulse(:),impulse_input(:)
	real, dimension(*) :: vein(:)
	integer :: epoch,active_data,grave
	character(len=*) :: direction
	integer :: column,row
	
	if (direction=="read") then
	
		!retrieve previous network
		open(unit=1,file="will.txt")
		do row=1,size(blood(1,1,:))
			do column=1,size(blood(1,:,1))
				read(1,*) blood(:,column,row)
			end do
		end do
		do row=1,size(brain(1,1,:))
			do column=1,size(brain(1,:,1))
				read(1,*) brain(:,column,row)
			end do
		end do
		read(1,*) vein
		read(1,*) impulse
		read(1,*) impulse_input
		read(1,*) epoch
		read(1,*) active_data
		read(1,*) grave
		close(1)
		
	else if (direction=="write") then
	
		!write the networks to file
		open(unit=2,file="will.txt")
		do row=1,size(blood(1,1,:))
			do column=1,size(blood(1,:,1))
				write(2,*) blood(:,column,row)
			end do
		end do
		do row=1,size(brain(1,1,:))
			do column=1,size(brain(1,:,1))
				write(2,*) brain(:,column,row)
			end do
		end do
		write(2,*) vein
		write(2,*) impulse
		write(2,*) impulse_input
		write(2,*) epoch
		write(2,*) active_data
		write(2,*) grave
		close(2)
	
	end if
	
end subroutine read_write






!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!        rung two          !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




subroutine initial(brain,blood,vein,impulse,impulse_input,epoch,active_data,grave,printed)
	integer,dimension(*),intent(inout) :: brain(:,:,:)
	real,dimension(*),intent(inout) :: blood(:,:,:)
	integer, dimension(*) :: impulse(:),impulse_input(:)
	real, dimension(*) :: vein(:)
	integer :: epoch,active_data,grave,maximum_columns
	character(len=*) :: printed
	integer :: print_length=7
	character(len=:),allocatable :: print_row, data_cha
	integer :: column,row,weight
	logical :: file_exists

	!set up maximum columns
	maximum_columns=size(brain(1,:,1))	
	
	!initialise printer
	allocate(character((maximum_columns+1)*print_length) :: print_row)
	allocate(character(print_length) :: data_cha)

	!first, check if there is column previous network that exists
	INQUIRE(FILE="will.txt", EXIST=file_exists)
	if (file_exists .eqv. .false.) then

		active_data=0
		grave=0
		epoch=1

		!initialize the blood weights
		do row=1,size(blood(1,1,:))
			do column=1,size(blood(1,:,1))
				do weight=1,size(blood(:,1,1))
					blood(weight,column,row)=0.01
				end do
			end do
		end do
		
		!initialise the action and input
		do weight=1,size(impulse_input)
			impulse_input(weight)=0
		end do
		do weight=1,size(impulse)
			impulse(weight)=0
		end do
		do weight=1,size(vein)
			vein(weight)=0.01
		end do
		
		!initialize the brain weights
		do row=1,size(brain(1,1,:))
			do column=1,size(brain(1,:,1))
				do weight=1,size(brain(:,1,1))
					brain(weight,column,row)=0
				end do
			end do
		end do

		!print the initial brain
		if (printed/="no") then 
			print'(A7)',"Brain 0"
			do row=1,size(blood(1,1,:))
				do column=1,size(blood(1,:,1))

					write(data_cha,"(F7.3)")blood(self_pos_blood(row,column,maximum_columns),column,row)
					print_row(column*print_length:column*print_length+print_length)=data_cha
					
				end do
				print *,print_row

			end do
			print*," "

		end if
		
	else

		!retrieve previous network
		call read_write(brain,blood,vein,impulse,impulse_input,epoch,active_data,grave,"read")
		
	end if
	
end subroutine initial



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Questions main subprograms!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!







!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Subprogram 1: the reward inducing continuous network is processed here!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine heart(brain,blood,epoch,active_data,grave,multiplier_scaling,printed_true)
implicit none

!input variables
character(len=*),intent(in) :: printed_true
character(len=12) :: printed

!timing objects
real ::  time_interval, start, finish, start_interval, finish_interval

!main brain objects
real,dimension(*) :: blood(:,:,:)
integer,dimension(*) :: brain(:,:,:)

!network objects
integer :: active_data,grave
integer :: maxim_column,maxim_row

!debugging objects
integer :: x

!incrementation objects
integer :: epoch, j,i,z, f,u,k, s,a,c, pos_hold, here_weight,here_column,here_row, there_weight,there_column,there_row
integer,allocatable :: matrix_pos(:)

real :: fate, transition, distil,multiplier_scaling

!printing objects
integer :: print_length=7
character(len=:),allocatable :: print_row, data_cha

!file checking
logical :: file_exists

!make this a new run
call random_seed()


call CPU_Time(start)

!setup print gates
if ((printed_true=="network_only") .or. (printed_true=="power_only")) then
	printed="no"
else
	printed=printed_true
end if

!establish maximums
maxim_column=size(brain(1,:,1));maxim_row=size(brain(1,1,:))

!initialise the randomised position marker arrays
allocate(matrix_pos(1:size(blood(:,1,1))))

!initialise printer
allocate(character(maxim_column*print_length+7) :: print_row)
allocate(character(print_length) :: data_cha)

!affect the blood with the brain multipliers
call electroviolence(brain,blood,multiplier_scaling)

!record the start time of the epoch
call CPU_Time(start_interval)


!this is the brain neuron action loop (main loop)

!the randomised loop initialiser - ensures data transition is not positionally dependant
call randomised_list(matrix_pos)

do s=1,size(blood(:,1,1))
	!take the randomised array of matrix positions and select a neuron
	here_row=point_pos_matrix_blood(matrix_pos(s),maxim_column,"row")
	here_column=point_pos_matrix_blood(matrix_pos(s),maxim_column,"column")
	here_weight=matrix_pos(s)	!k is the z position of the current matrix element represented by j and i	

	do there_weight=1,size(blood(:,1,1))
	
		there_row=point_pos_matrix_blood(there_weight,maxim_column,"row")	!f is the j position of the current matrix element represented by z
		there_column=point_pos_matrix_blood(there_weight,maxim_column,"column")	!u is the i position of the current matrix element represented by z

		!the first condition stops the neuron from acting on itself
		!the second condition skips dead neurons
		if ((here_weight/=there_weight) .and. (blood(here_weight,here_column,here_row)/=0.)) then

			call neuron_fire(blood,there_weight,there_column,here_row,here_weight,here_column,there_row)

		end if
	end do

end do

call CPU_Time(finish_interval)
time_interval=finish_interval-start_interval


!print the brain
if ((printed=="yes") .or. (printed=='debug')) then
	print'(A6,I0)',"Blood ",epoch
	do s=1,size(blood(1,1,:))
		do a=1,size(blood(1,:,1))

			write(data_cha,"(F7.3)")blood(self_pos_blood(s,a,maxim_column),a,s)
			print_row(a*print_length:a*print_length+7)=data_cha
			
		end do
		print *,print_row

	end do
	!print*," "

end if

!enact time penalty on each neuron
!this is a key part of the system's learning power
!this ensures against runaway neuron growth and also limits growth of the brain past a point where neuron action takes too long
do z=1,size(blood(1,1,:))
	do i=1,size(blood(1,:,1))
		do j=1,size(blood(:,1,1))
			blood(j,i,z)=blood(j,i,z)*(1-time_interval)
		end do
	end do
end do

!end timer
call CPU_Time(finish)
if ((printed=="yes") .or. (printed=='debug')) then
	call print_interval(start,finish)
	print*," "
end if
end subroutine heart








!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Subprogram 2: the discrete unitary network is processed here!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine head(brain,blood,impulse,valve_value,active_data,grave,multiplier_scaling,epoch,printed_true)

implicit none

!printing and variable input objects
character(len=2) :: data_cha
character(len=3) :: data_cha_freeze
character(len=*),intent(in) :: printed_true
character(len=12) :: printed,valves="down"
character(len=10000) :: error_num
character(len=:),allocatable :: print_row,print_row_freeze

!incrementation and limiting objects
real :: multiplier_scaling
integer,parameter :: data_limiter=100
integer :: epoch,here_row,here_weight,there_row,there_column,here_column,h,k,g !g=multi_pos, h=conflict numerator
logical :: test_overlap_conflict, test_condition_conflict
integer,allocatable :: row_random(:),column_random(:)
integer,dimension(2) :: j_i

!network objects
integer, dimension(*) :: brain(:,:,:)
integer,allocatable :: brain_freeze(:,:) !brain_freeze stores a self pos value that gives the address that the data at position in the matrix corresponding to brain should go
real,dimension(*) :: blood(:,:,:)

!action objects
integer, dimension(*) :: impulse(:)

!debugging
integer :: counter
integer :: neuron_row=10,neuron_column=8

!time and chance
real :: fuck,start,finish

!boundary objects
integer :: valve_value
integer,dimension(4) :: boundaries !(bottom, left, right, top)

!control objects
integer :: cycles,maximum_columns,maximum_rows,active_data,grave

!for the time record          
call CPU_Time(start)

!make this a new run
call random_seed()

!setup print gates
if (printed_true=="network_only") then
	printed="yes"
else if (printed_true=="power_only") then
	printed="no"
else
	printed=printed_true
end if

!set the network limits
maximum_columns=size(brain(1,:,1));maximum_rows=size(brain(1,1,:))

!set the arrays
allocate(brain_freeze(maximum_columns,maximum_rows))
allocate(character(maximum_columns*2+1) :: print_row)
allocate(character(maximum_columns*3+2) :: print_row_freeze)
allocate(row_random(maximum_rows))
allocate(column_random(maximum_columns))	

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
			j_i=point_pos_matrix_brain(here_weight,maximum_columns)
			print'(A6,I2,A1,I2,A4,I0)',"For (",j_i(1),",",j_i(2),") p:",brain(here_weight,neuron_column,neuron_row)
		end if
	end do
	print*," "
end if

!count how much data is in brain
active_data=0
do there_row=1,size(brain(1,1,:))
	do there_column=1,size(brain(1,:,1))
		if (brain(self_pos_brain(there_row,there_column,maximum_columns),there_column,there_row)==1) then
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
		if (brain(self_pos_brain(row_random(here_row),column_random(here_column),maximum_columns),&
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
				if (brain_freeze(here_column,here_row)==self_pos_brain(1,there_column,maximum_columns)) then
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

				!after 20 tries, clearly something is stuck, so just skip problem neurons
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
								else if ((brain_freeze(here_column,here_row)==self_pos_brain(there_row,there_column,maximum_columns)) .and. &
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


	counter=counter+1
	
end do

!debuggling: print brain_freeze 
if (printed=='debug') then
	print*,"Brain Freeze After",epoch
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
			if ((brain(h,there_column,there_row)>0) .and. (self_pos_brain(there_row,there_column,maximum_columns)/=h)) then
				brain(h,there_column,there_row)=brain(h,there_column,there_row)-(1+((brain(h,there_column,there_row))/data_limiter))
			end if
		end do
	end do
end do	

!count how much data is in brain
active_data=0
do there_row=1,size(brain(1,1,:))
	do there_column=1,size(brain(1,:,1))
		if (brain(self_pos_brain(there_row,there_column,maximum_columns),there_column,there_row)==1) then
			active_data=active_data+1
		end if
	end do
end do

!if requested a printout, print brain after the transitions are completed
if ((printed=='yes') .or. (printed=='debug')) then
	print'(A6,I0,A14,I0,A12,I0)',"Brain ",epoch," active data: ",active_data," dead data: ",grave
	do there_row=1,size(brain(1,1,:))
		do there_column=1,size(brain(1,:,1))
			write(data_cha,"(I2)")brain(self_pos_brain(there_row,there_column,maximum_columns),there_column,there_row)
			print_row(there_column*2-1:there_column*2)=data_cha
		end do
		print *,print_row
	end do
	!print*," "
end if

!debuggling: check if any neurons are greater than 1
do here_row=1,size(brain(1,1,:))
	do here_column=1,size(brain(1,:,1))
		if ((brain(self_pos_brain(here_row,here_column,maximum_columns),here_column,here_row)>1) .or. &
			(brain(self_pos_brain(here_row,here_column,maximum_columns),here_column,here_row)<0)) then
			
			stop
			
		end if
	end do
end do


call CPU_Time(finish)
if ((printed=="yes") .or. (printed=='debug')) then
	call print_interval(start,finish)
	print*," "
end if

end subroutine head




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Subprogram 3: all the input and output of the network and special reward is enacted here!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine strength(brain,blood,vein_action,impulse_action,impulse_input,epoch,cycles,scaling,cat_angle,printed_true)

implicit none

!network read/write
integer, dimension(*) :: brain(:,:,:)
real,dimension(*) :: blood(:,:,:)
integer :: epoch,active_data,grave
character(len=1000) :: maxim_column_cha,maxim_row_cha,network_scaling_cha,cyclical_cha
character(len=*),intent(in) :: printed_true
character(len=12) :: printed

!printing
character(len=:),allocatable :: print_row_impulse_action,print_row_impulse_input
character(len=2) :: data_cha_impulse_action,data_cha_impulse_input
integer :: print_length_vein_action=7
character(len=:),allocatable :: print_row_vein_action,data_cha_vein_action

!action objects
integer, dimension(*) :: impulse_action(:),impulse_input(:)
real, dimension(*) :: vein_action(:)
real :: transition,vein_change,cat_angle
integer :: shift,shift_total,shift_max,cycles

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

!setup print gates
if (printed_true=="network_only") then
	printed="no"
else if (printed_true=="power_only") then
	printed="yes"
else
	printed=printed_true
end if

!set network limits
maxim_row=size(brain(1,1,:));maxim_column=size(brain(1,:,1))

!allocate the networks
allocate(character((maxim_column)*2+1) :: print_row_impulse_input)
allocate(character((maxim_column+2)*2+1) :: print_row_impulse_action)
allocate(character((maxim_column+2)*print_length_vein_action+7) :: print_row_vein_action)
allocate(character(print_length_vein_action) :: data_cha_vein_action)

!test1: keep data in impulse_input in the centre (odd columns only)

!move the impulse_input based on the impulse_action
!at the start, set input in the middle
if (epoch<=1) then

	do here_column=1,size(impulse_input)
		impulse_input(here_column)=0
	end do 
	impulse_input((size(impulse_input)/2)+1)=1
	input_here_column=(size(impulse_input)/2)+1	
	shift=0
	
else if ((epoch<2000) .and. (mod(epoch,250)==0)) then

	do here_column=1,size(impulse_input)
		impulse_input(here_column)=0
	end do 
	impulse_input((size(impulse_input)/2)+1)=1
	input_here_column=(size(impulse_input)/2)+1

else if ((epoch>=2000) .and. (epoch<10000) .and. (mod(epoch,2000)==0)) then

	do here_column=1,size(impulse_input)
		impulse_input(here_column)=0
	end do 

	impulse_input(size(impulse_input))=1
	input_here_column=size(impulse_input)
else if ((epoch>=2000) .and. (epoch<10000) .and. (mod(epoch,1000)==0)) then

	do here_column=1,size(impulse_input)
		impulse_input(here_column)=0
	end do 

	impulse_input(1)=1
	input_here_column=1

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
vein_action(2*((size(impulse_input)/2)+1)-input_here_column+1)=vein_action(2*((size(impulse_input)/2)+1)-input_here_column+1)&
	+10.0*(1.0/(float(abs(((size(impulse_input)/2)+1)-input_here_column))+1.0))
	

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

!remove impulse_action data
do here_column=1,size(impulse_action)
	impulse_action(here_column)=0
end do	

!save the shift
if (epoch==cycles) then
	print"(I0)",shift
end if
!open(unit=3, file="shift.txt")
!write(3,*)shift
close(3)

!stop timer and print
call cpu_time(finish)

!if requested a printout, print impulse_action and vein_action after the transitions are completed
if ((printed=='yes') .or. (printed=='debug')) then

	print'(A14,I0)',"impulse_input ",epoch
	do there_column=1,size(impulse_input)
		write(data_cha_impulse_input,"(I2)")impulse_input(there_column)
		print_row_impulse_input(there_column*2-1:there_column*2)=data_cha_impulse_input
	end do
	print *,print_row_impulse_input
	print*," "

	print'(A15,I0)',"impulse_action ",epoch
	do there_column=1,size(impulse_action)
		write(data_cha_impulse_action,"(I2)")impulse_action(there_column)
		print_row_impulse_action(there_column*2-1:there_column*2)=data_cha_impulse_action
	end do
	print *,print_row_impulse_action
	print*," "

	print'(A12,I0)',"vein_action ",epoch
	do there_column=1,size(vein_action)
		write(data_cha_vein_action,"(F7.3)")vein_action(there_column)
		print_row_vein_action(there_column*print_length_vein_action:there_column*print_length_vein_action+7)=data_cha_vein_action
	end do
	print *,print_row_vein_action
	print*," "

	print*,"Shift Value:",shift
	print*," "
	
	print*,"Reward magnitude: ",10.0*(1.0/(float(abs(((size(impulse_input)/2)+1)-input_here_column))+1.0))
	print*," "
	
	call print_interval(start,finish)
	print*," "
	
end if



end subroutine strength







end module answers
