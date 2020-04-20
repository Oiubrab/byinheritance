program test_random
implicit none

real :: believe
character(len=15) :: filename="random_test.txt"
integer :: i

open(unit=10,file=filename)
do i=1,10000
	call random_number(believe)
	write(10,*) believe
end do
close(10)

end program test_random
