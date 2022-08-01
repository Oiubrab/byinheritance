program cenotaph
use in_search_of_sanity
use welcome_to_dying
use spiritechnology
use the_sound_of_perserverance
implicit none

!the network
type(mind),dimension(5) :: think
logical,parameter :: motivator=.true.,test=.false.
logical :: initial=.true.
real,parameter :: blood_volume=0.8,blood_gradient=0.6
integer,parameter :: vision_socket=7,response_socket=7,blood_rate=20,epoch_cutoff=75
integer,parameter :: neurodepth=10,columns=15,rows=10
integer :: oddsey,repeater,num_img,image_number
real :: node_use_reward=10.0,finish,start
integer,dimension(10),codimension[*] :: vision,response
num_img = num_images()
image_number = this_image ()




allocate(think%brain_status(2,columns,rows))
allocate(think%brain_weight(8,8,columns,rows))
allocate(think%blood(columns,rows+1))
allocate(think%neurochem(2,neurodepth,columns,rows))

allocate(transfer_array(10*num_img)

call cpu_time(start)
do repeater=1,153
	vision=[0,0,0,0,1,0,0,0,0,0]

	if (repeater == 1) then 
		initial=.true. 
	else 
		initial=.false. 
	end if
	
	if (image_number==1) then
		print*," "
		print'(10I2)',vision
		print*," "
	end if
	
	call insanitorium_deluxe(initial,think,oddsey,image_number,node_use_reward,&
		vision,response,vision_socket,response_socket,blood_rate,&
		blood_volume,blood_gradient,epoch_cutoff,motivator,test)
		
	if (image_number==1) then
		print*," "
		print'(10I2)',response
		print*," "
	end if
	
	
	
	sync all
	if (image_number==1) then
		vision(6:10)[num_img]=response(1:5)
		vision(1:5)[1]=response(6:10)
	else
		vision(6:10)[image_number-1]=response(1:5)
		vision(1:5)[image_number]=response(6:10)
	end if
	
end do
call cpu_time(finish)

print*,finish-start


end program
