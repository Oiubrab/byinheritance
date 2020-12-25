!this program is a simple echo it's arguments
!it is meant to show me what the eudm is outputting to in_search_of_sanity
program test_output
implicit none

character(len=1000) :: arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9

CALL GET_COMMAND_ARGUMENT(1,arg1)	
CALL GET_COMMAND_ARGUMENT(2,arg2)
CALL GET_COMMAND_ARGUMENT(3,arg3)
CALL GET_COMMAND_ARGUMENT(4,arg4)	
CALL GET_COMMAND_ARGUMENT(5,arg5)
CALL GET_COMMAND_ARGUMENT(6,arg6)
CALL GET_COMMAND_ARGUMENT(7,arg7)	
CALL GET_COMMAND_ARGUMENT(8,arg8)
CALL GET_COMMAND_ARGUMENT(9,arg9)						

print*,"arg1 is:",trim(arg1)
print*,"arg2 is:",trim(arg2)
print*,"arg3 is:",trim(arg3)
print*,"arg4 is:",trim(arg4)
print*,"arg5 is:",trim(arg5)
print*,"arg6 is:",trim(arg6)
print*,"arg7 is:",trim(arg7)
print*,"arg8 is:",trim(arg8)
print*,"arg9 is:",trim(arg9)

end program test_output
