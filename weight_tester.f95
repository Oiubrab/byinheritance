program weight_reducer
implicit none

!this program ia a cunt. It counts how many steps a weight would take to get back to unity, assuming no further stimulation


integer :: counter
real :: weight_max=1000000.0, weight_max_holder
real,parameter :: first_height=1.0-(27.825/34.0), first_gradient=27.825/34.0 !1st stage linear parameters
real,parameter :: height=-3.3, gradient=1.0 !2nd stage linear parameters
real,parameter :: period=2.3,amplitude=-2.4 !sinusoid parameters
real,parameter :: second_period_inverse=10.0,second_amplitude=0.0008,second_sin_shift=300.0 !2nd sinusoid, 2nd stage, parameters
real,parameter :: normal_height=4.3,normal_distance=2.6,normal_width=5.205 !gaussian parameters
real,parameter :: overload=5.0 !over 1050 constant reduction

weight_max_holder=0.0
counter=0
do while (weight_max>1.1)
	!stop when we reach the bottom
	if (weight_max==weight_max_holder) then
		exit
	end if
	weight_max_holder=weight_max
	print*,weight_max,counter
	if (weight_max>1.) then
		!between 1 and 35, scaling is linear
		if (weight_max<=35.0) then
			weight_max=first_gradient*weight_max+first_height
		!between 35 and 1050, scaling is a non linear function
		else if ((weight_max>35.0) .and. (weight_max<1050.0)) then
			!weight is scaled down by a gaussian (smalll numbers), plus a linear (general shape), plus a quadratic (limiter), plus a sinusoid (variation)
			!range within the effective domain (1,big) must stay below the y=x line and above the y=1 line 
			weight_max=amplitude*sin(0.1*period*weight_max)+&
				gradient*weight_max+height-&
				second_amplitude*sin((weight_max+second_sin_shift)/second_period_inverse)+&
				normal_height*exp(-1.0*(((weight_max-normal_distance)/normal_width)**2))
		!if weight is over 1050, just do a constant reduction
		else
			weight_max=weight_max-overload
		end if
	end if
	counter=counter+1
end do

end program weight_reducer
