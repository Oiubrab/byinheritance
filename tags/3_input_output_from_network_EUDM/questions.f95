program questions
use discrete_flesh
use flesh_out
use flesh
use answers
implicit none

!program command line input
character(len=1000) :: valve_value_cha, cycles_cha, maximum_columns_cha,angle_from_cat_cha
character(len=1000) :: maximum_rows_cha, lag_cha, blood_scaling_cha, network_scaling_cha

!constants
real,parameter :: pi=4.*asin(1./sqrt(2.))
real :: slice

!valve values
integer :: valve_value, cycles, lag

!network dimensions
integer :: maximum_columns,maximum_rows

!network variation
real :: blood_scaling,network_scaling
integer :: active_data,grave,epoch,gravebefore

!printing
character(len=12) :: printed
logical :: file_exists

!main brain objects
real,allocatable :: blood(:,:,:)
integer,allocatable :: brain(:,:,:)

!action objects
logical :: ending
integer, allocatable :: impulse(:),impulse_input(:)
real, allocatable :: vein(:)
real :: angle_from_cat
logical :: starter
integer :: shift

!clock
real :: start,finish

!for the time record          
call CPU_Time(start)

!prepare command line options
IF(COMMAND_ARGUMENT_COUNT().NE.9)THEN
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program angle_from_cat valve_value cycles maximum_columns maximum_rows lag blood_scaling network_scaling printed'
	WRITE(*,*) "printed: yes no debug network_only power_only"
	WRITE(*,*) "blood_scaling: scale how much the brain neuron will cause the blood neuron to attract more blood"
	WRITE(*,*) "network_scaling: scales the amount blood neurons will increase the weights that lead to brain neurons"
	STOP
ENDIF
!set the input variables
CALL GET_COMMAND_ARGUMENT(1,angle_from_cat_cha)
CALL GET_COMMAND_ARGUMENT(2,valve_value_cha)
CALL GET_COMMAND_ARGUMENT(3,cycles_cha)
CALL GET_COMMAND_ARGUMENT(4,maximum_columns_cha)
CALL GET_COMMAND_ARGUMENT(5,maximum_rows_cha)
CALL GET_COMMAND_ARGUMENT(6,lag_cha)
CALL GET_COMMAND_ARGUMENT(7,blood_scaling_cha)
CALL GET_COMMAND_ARGUMENT(8,network_scaling_cha)
CALL GET_COMMAND_ARGUMENT(9,printed)
READ(angle_from_cat_cha,*)angle_from_cat
READ(valve_value_cha,*)valve_value
READ(cycles_cha,*)cycles
READ(maximum_columns_cha,*)maximum_columns
READ(maximum_rows_cha,*)maximum_rows
READ(lag_cha,*)lag
READ(blood_scaling_cha,*)blood_scaling
READ(network_scaling_cha,*)network_scaling

!initialise the two networks
allocate(blood(maximum_rows*maximum_columns,maximum_columns,maximum_rows))
allocate(brain((maximum_columns+2)*(maximum_rows+2),maximum_columns,maximum_rows))

!initialise the action arrays
allocate(vein(maximum_columns+2))
allocate(impulse(maximum_columns+2))
allocate(impulse_input(maximum_columns))

!if this is the first time this network is activated, it has to be initialised
INQUIRE(FILE="will.txt", EXIST=file_exists)
if (file_exists .eqv. .true.) then
	call read_write(brain,blood,vein,impulse,impulse_input,epoch,active_data,grave,"read")
else
	call initial(brain,blood,vein,impulse,impulse_input,epoch,active_data,grave,printed)
end if

!run the network subroutines
starter=.true.
ending=.false.
do while (ending .eqv. .false.)
	gravebefore=grave
	call heart(brain,blood,epoch,active_data,grave,blood_scaling,printed)
	call head(brain,blood,valve_value,active_data,grave,network_scaling,epoch,printed)
	call strength(brain,blood,vein,impulse,impulse_input,epoch,&
		grave,shift,network_scaling,angle_from_cat,ending,starter,printed)
	!if data leaves the network, end
	if (gravebefore/=grave) then
		ending=.true.
		print"(I0)",shift
	end if
	!lag it if necessary
	call sleep(lag)
	!if starter is true, sample from the outside world is taken
	starter=.false.
	epoch=epoch+1
end do

!write the network for next loop if the correct answer is given
slice=(2.*pi)/float(size(impulse_input))
if (((shift<0) .and. (angle_from_cat>slice)) .or. ((shift>0) .and. (angle_from_cat<(-1.0*slice)))&
	.or. ((shift==0) .and. (abs(angle_from_cat)<slice))) then
	call read_write(brain,blood,vein,impulse,impulse_input,epoch,active_data,grave,"write")
else
	print*,"nope"
end if

if (printed/="no") then
	!stop timer and print
	call cpu_time(finish)
	print*,"Total time elapsed:"
	call print_interval(start,finish)
	print*," "
end if

end program