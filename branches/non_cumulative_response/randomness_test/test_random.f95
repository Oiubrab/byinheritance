program test_random
implicit none

real :: believe, translation, translation_left, translation_right, translation_common, stretch, unitary, mode
real,parameter :: pi=4.*asin(1./sqrt(2.))
character(len=15) :: filename="random_test.txt"
integer :: i

!this is the number which will occur most often
mode=0.6

!this is the translation necessary to make the modified believe set below an element of (0,1)
translation_common=(2*mode**3)-(3*mode**2)+sqrt(mode**4-2*mode**3+mode**2)+mode
translation_left=(translation_common/2)**(1./3.)
translation_right=((2**(1./3.))*(9*mode-9*mode**2))/(9*translation_common**(1./3.))
translation=translation_left-translation_right+mode

!this is the stretching necessary to make the modified believe set below an element of (0,1)
unitary=((1-translation)**3)+translation**3
stretch=1./unitary

open(unit=10,file=filename)
do i=1,100000
	call random_number(believe)
	believe=(stretch*(believe-translation)**3)+mode
	write(10,*) believe
end do
close(10)

end program test_random
