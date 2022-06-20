program cenotaph
use in_search_of_sanity
use welcome_to_dying
use spiritechnology
use the_sound_of_perserverance
implicit none

!the network
type(mind) :: think
logical,parameter :: motivator=.false.,test=.false.
logical :: initial=.true.
real,parameter :: blood_volume=0.8,blood_gradient=0.6
integer,parameter :: image_number=1,vision_socket=7,response_socket=7,blood_rate=20,epoch_cutoff=75
integer,parameter :: neurodepth=10,columns=15,rows=10
integer :: oddsey,repeater
real :: node_use_reward=10.0,finish,start
integer,dimension(10) :: vision,response

vision=[0,0,0,0,1,0,0,0,0,0]

allocate(think%brain_status(2,columns,rows))
allocate(think%brain_weight(8,8,columns,rows))
allocate(think%blood(columns,rows+1))
allocate(think%neurochem(2,neurodepth,columns,rows))

call cpu_time(start)
do repeater=1,153
	call insanitorium_deluxe(initial,think,oddsey,image_number,node_use_reward,&
		vision,response,vision_socket,response_socket,blood_rate,&
		blood_volume,blood_gradient,epoch_cutoff,motivator,test)
end do
call cpu_time(finish)

print*,finish-start
print'(10I2)',vision
print'(10I2)',response

end program
