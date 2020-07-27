program questions
use discrete_flesh
use flesh_out
use flesh
use answers
implicit none

!program command line input
character(len=1000) :: valve_value_cha, cycles_cha, maximum_columns_cha
character(len=1000) :: maximum_rows_cha, lag_cha, blood_scaling_cha, network_scaling_cha

!valve values
character(len=6) :: valves
integer :: valve_value, cycles, lag

!network dimensions
integer :: maximum_columns,maximum_rows

!network variation
real :: blood_scaling,network_scaling
integer :: active_data,grave,epoch

!printing
character(len=12) :: printed

!main brain objects
real,allocatable :: blood(:,:,:)
integer,allocatable :: brain(:,:,:)

!action objects
integer, allocatable :: impulse(:),impulse_input(:)
real, allocatable :: vein(:)

!clock
real :: start,finish

!for the time record          
call CPU_Time(start)

!prepare command line options
IF(COMMAND_ARGUMENT_COUNT().NE.9)THEN
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program valves valve_value cycles maximum_columns maximum_rows lag blood_scaling network_scaling printed'
	WRITE(*,*) "valves: left right up down custom"
	WRITE(*,*) "printed: yes no debug network_only power_only"
	WRITE(*,*) "network_scaling: scales the amount blood neurons will increase the weights that lead to brain neurons"
	WRITE(*,*) "blood_scaling: scale how much the brain neuron will cause the blood neuron to attract more blood"
	STOP
ENDIF
!set the input variables
CALL GET_COMMAND_ARGUMENT(1,valves)
CALL GET_COMMAND_ARGUMENT(2,valve_value_cha)
CALL GET_COMMAND_ARGUMENT(3,cycles_cha)
CALL GET_COMMAND_ARGUMENT(4,maximum_columns_cha)
CALL GET_COMMAND_ARGUMENT(5,maximum_rows_cha)
CALL GET_COMMAND_ARGUMENT(6,lag_cha)
CALL GET_COMMAND_ARGUMENT(7,blood_scaling_cha)
CALL GET_COMMAND_ARGUMENT(8,network_scaling_cha)
CALL GET_COMMAND_ARGUMENT(9,printed)
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
call initial(brain,blood,vein,impulse,impulse_input,epoch,active_data,grave,printed)

!run the network subroutines
do epoch=epoch,epoch+cycles
	call heart(brain,blood,epoch,active_data,grave,blood_scaling,printed)
	call head(brain,blood,impulse,valves,valve_value,active_data,grave,network_scaling,epoch,printed)
	call strength(brain,blood,vein,impulse,impulse_input,epoch,cycles,network_scaling,printed)
	!lag it if necessary
	call sleep(lag)
end do



!stop timer and print
call cpu_time(finish)
print*,"Total time elapsed:"
call print_interval(start,finish)
print*," "

end program
