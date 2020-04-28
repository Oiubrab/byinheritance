program test_random
implicit none

real,parameter :: pi=4.*asin(1./sqrt(2.))
integer,parameter :: neurons=8
real,dimension(neurons) ::  translation, translation_left, translation_right, translation_common, stretch, unitary, mode, believe
real,dimension(neurons) ::  foldiness, foldiness_inverse, foldiness_mode_three, foldiness_mode_nine, fractional, rooted_fraction, &
	cube_root, long_fraction, why_zero, why_one, ex_zero, ex_one, gradient, intercept
character(len=15) :: filename="random_test.txt"
character(len=5) :: types
integer :: i,j

types='solve'


!this is the number which will occur most often
mode=(/0.0625,0.1875,0.3125,0.4375,0.5625,0.6875,0.8125,0.9375/)
print*,mode

if (types=='simpl') then

	!this is the translation necessary to make the modified believe set below an element of (0,1)
	translation_common=(2*mode**3)-(3*mode**2)+sqrt(mode**4-2*mode**3+mode**2)+mode
	translation_left=(translation_common/2)**(1./3.)
	translation_right=((2**(1./3.))*(9*mode-9*mode**2))/(9*translation_common**(1./3.))
	translation=translation_left-translation_right+mode
	print*,translation

	!this is the stretching necessary to make the modified believe set below an element of (0,1)
	unitary=((1-translation)**3)+translation**3
	stretch=1./unitary
	print*,stretch

	open(unit=10,file=filename)
	do i=1,10000
		call random_number(believe)
		believe=(stretch*(believe-translation)**3)+mode
		do j=1,neurons
			write(10,*) believe(j)
		end do
	end do
	close(10)

else if (types=='exact') then

	!this is the number which will determine the relative likelihood of the number falling in the bin 
	foldiness_inverse=(/1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0/)
	foldiness=1/foldiness_inverse

	!setting up the first level of common factors
	foldiness_mode_three=-3*(foldiness**2)+3*foldiness-9*(mode**2)+9*mode
	foldiness_mode_nine=54*(foldiness**2)*mode-27*(foldiness**2)+54*(mode**3)-81*(mode**2)+27*mode
	fractional=(foldiness+mode)/(2*foldiness+1)
	rooted_fraction=1/(3*(2**(1./3.))*(2*foldiness+1))

	!setting up the second level of common factors
	cube_root=rooted_fraction*(((4*(foldiness_mode_three**3)+foldiness_mode_nine**2)**(1./2.))+foldiness_mode_nine)**(1./3.)

	!setting up the third level of common factors
	long_fraction=((2**(1./3.))*foldiness_mode_three)/(3*(2*foldiness+1)*cube_root)

	!this is the translation necessary to make the modified believe set below an element of (0,1)
	translation=rooted_fraction*cube_root-long_fraction+fractional
	print*,translation

	!this is the stretching necessary to make the modified believe set below an element of (0,1)
	!stretch=12*foldiness*(translation**2)+6*(translation**2)-12*foldiness*translation-18*&
	!	mode*translation+3*translation-foldiness+9*mode+1
	stretch=(1-foldiness)/((3*translation**2)-3*translation+1)
	print*,stretch

	open(unit=10,file=filename)
	do i=1,10000
		call random_number(believe)
		believe=(stretch*(believe-translation)**3)+foldiness*(believe-translation)+mode
		do j=2,2!neurons
			write(10,*) believe(j)
		end do
	end do
	close(10)

else if (types=='solve') then

	!this is the number which will determine the relative likelihood of the number falling in the bin 
	foldiness_inverse=(/100.0,10.0,10.0,100.0,10.0,10.0,10.0,10.0/)
	foldiness=1/foldiness_inverse

	!this is the translation necessary to make the modified believe set below an element of (0,1)
	translation_common=(2*mode**3)-(3*mode**2)+sqrt(mode**4-2*mode**3+mode**2)+mode
	translation_left=(translation_common/2)**(1./3.)
	translation_right=((2**(1./3.))*(9*mode-9*mode**2))/(9*translation_common**(1./3.))
	translation=translation_left-translation_right+mode
	print*,translation

	!this is the stretching necessary to make the modified believe set below an element of (0,1)
	unitary=((1-translation)**3)+translation**3
	stretch=1./unitary
	print*,stretch

	why_zero=-stretch*(translation**3)-foldiness*translation+mode
	why_one=(stretch*(1-translation)**3)+foldiness*(1-translation)+mode
	gradient=1/(why_one-why_zero)
	intercept=-why_zero/(why_one-why_zero)

	open(unit=10,file=filename)
	do i=1,1000
		call random_number(believe)
		believe=(stretch*(believe-translation)**3)+foldiness*(believe-translation)+mode
		believe=(believe-why_zero)/(why_one-why_zero)
		do j=1,neurons
			write(10,*) believe(j)
		end do
	end do
	close(10)

end if

end program test_random
