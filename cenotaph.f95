program cenotaph
use in_search_of_sanity
use welcome_to_dying
use spiritechnology
use the_sound_of_perserverance
implicit none

!the network
type(mind),dimension(5) :: think
type(mind) :: motivate
logical,parameter :: motivator=.true.,test=.false.
logical :: initial=.true.
real,parameter :: blood_volume=0.8,blood_gradient=0.6
integer,parameter :: vision_socket=7,response_socket=7,blood_rate=20,epoch_cutoff=75
integer,parameter :: neurodepth=10,columns=15,rows=10,vision_response_size=10
integer :: oddsey,repeater,num_img,image_number,layer,slot,choice
real :: node_use_reward=10.0,finish,start,pick
integer,dimension(vision_response_size),codimension[*] :: vision,response
integer,allocatable,codimension[:] :: shotgun(:)
num_img = num_images()
image_number = this_image ()



do layer=1,5
	allocate(think(layer)%brain_status(2,columns,rows))
	allocate(think(layer)%brain_weight(8,8,columns,rows))
	allocate(think(layer)%blood(columns,rows+1))
	allocate(think(layer)%neurochem(2,neurodepth,columns,rows))
end do

allocate(shotgun(num_img*10)[*])

call cpu_time(start)
do repeater=1,153
	!random data
	vision=[0,0,0,0,0,0,0,0,0,0]
	call random_number(pick)
	slot=(pick*vision_response_size)+1
	do choice=1,vision_response_size
	if( choice==slot ) then
		vision(choice)=1
	end if
	
	end do
	do layer=1,5
		if (repeater == 1) then 
			initial=.true. 
		else 
			initial=.false. 
		end if
		
		!vision printer
!		if (image_number==1) then
!			print*," "
!			print'(10I2)',vision
!			print*," "
!		end if
		
		call insanitorium_deluxe(initial,think(layer),oddsey,image_number,node_use_reward,&
			vision,response,vision_socket,response_socket,blood_rate,&
			blood_volume,blood_gradient,epoch_cutoff,motivator,test)
			
		!response printer
!		if (image_number==1) then
!			print*," "
!			print'(10I2)',response
!			print*," "
!		end if
		
		
		!transfer all the response layers across to vision layers
		!stagger the transfer so that data has a chance to mix
		sync all
		if (image_number==1) then
			vision(6:10)[num_img]=response(1:5)
			vision(1:5)[1]=response(6:10)
		else
			vision(6:10)[image_number-1]=response(1:5)
			vision(1:5)[image_number]=response(6:10)
		end if
	end do
	sync all
	!pull everything together into an output
	shotgun(1+10*(image_number-1):10+10*(image_number-1))[1]=response
	if (image_number==1) then
		print"(80I2)",shotgun
	end if
end do
call cpu_time(finish)

print*,finish-start


end program
