program test_network
use flesh
implicit none

real,parameter :: pi=4*asin(1./sqrt(2.))
integer :: thrash,i,j,k
integer,parameter :: maximum_columns=5,maximum_rows=5
integer, dimension(25,maximum_columns,maximum_rows) :: brain
real :: fuck,me
character(len=16) :: formatte="(I2,I2,I2,I2,I2)"

do i=1,size(brain(1,1,:))
	do j=1,size(brain(1,:,1))
		do k=1,size(brain(:,1,1))
			brain(k,j,i)=0
		end do
	end do
end do

do thrash=0,15

	if ((-1)**((thrash/maximum_rows)+1)==-1) then
		brain(mod(thrash,maximum_columns)+1,mod(thrash,maximum_columns)+1,1)=1
	else
		brain(maximum_columns-mod(thrash,maximum_columns),maximum_columns-mod(thrash,maximum_columns),1)=1	
	end if	
	
	print*,"Brain Before",thrash+1
	do i=1,size(brain(1,1,:))
		print formatte,brain(self_pos(i,1,maximum_columns),1,i),brain(self_pos(i,2,maximum_columns),2,i),&
			brain(self_pos(i,3,maximum_columns),3,i),brain(self_pos(i,4,maximum_columns),4,i),brain(self_pos(i,5,maximum_columns),5,i)
	end do

	do i=1,size(brain(1,1,:))
		do j=1,size(brain(1,:,1))
			do k=1,size(brain(:,1,1))
				if ((k==self_pos(i,j,maximum_columns)) .and. (brain(k,j,i)==1) .and. ((i/=1) .or. &
					(i/=maximum_rows)) .and. ((j/=1) .or. (j/=maximum_columns))) then

					brain(k,j,i)=0
					call RANDOM_NUMBER(fuck)
					if (fuck<0.125) then
						brain(self_pos(i-1,j-1,maximum_columns),j-1,i-1)=1
					else if (fuck<0.25) then
						brain(self_pos(i-1,j,maximum_columns),j,i-1)=1
					else if (fuck<0.375) then
						brain(self_pos(i-1,j+1,maximum_columns),j+1,i-1)=1
					else if (fuck<0.5) then
						brain(self_pos(i,j-1,maximum_columns),j-1,i)=1					
					else if (fuck<0.625) then
						brain(self_pos(i,j+1,maximum_columns),j+1,i)=1
					else if (fuck<0.75) then
						brain(self_pos(i+1,j-1,maximum_columns),j-1,i+1)=1
					else if (fuck<0.875) then
						brain(self_pos(i+1,j,maximum_columns),j,i+1)=1
					else
						brain(self_pos(i+1,j+1,maximum_columns),j+1,i+1)=1
					end if
					print*,"case1"


				! first set of cases - on the border of the matrix but not in the corner

				else if ((k==self_pos(i,j,maximum_columns)) .and. (brain(k,j,i)==1) .and. (i==1) .and. ((j/=1) .or. (j/=maximum_columns))) then
					
					brain(k,j,i)=0
					call RANDOM_NUMBER(fuck)
					if (fuck<0.2) then
						brain(self_pos(i,j-1,maximum_columns),j-1,i)=1
					else if (fuck<0.4) then
						brain(self_pos(i,j+1,maximum_columns),j+1,i)=1
					else if (fuck<0.6) then
						brain(self_pos(i+1,j-1,maximum_columns),j-1,i-1)=1
					else if (fuck<0.8) then
						brain(self_pos(i+1,j,maximum_columns),j,i+1)=1					
					else
						brain(self_pos(i+1,j+1,maximum_columns),j+1,i+1)=1
					end if
					print*,"case2"
				else if ((k==self_pos(i,j,maximum_columns)) .and. (brain(k,j,i)==1) .and. (i==maximum_rows) .and. &
					((j/=1) .or. (j/=maximum_columns))) then
					
					brain(k,j,i)=0
					call RANDOM_NUMBER(fuck)
					if (fuck<0.2) then
						brain(self_pos(i-1,j-1,maximum_columns),j-1,i-1)=1
					else if (fuck<0.4) then
						brain(self_pos(i-1,j,maximum_columns),j,i-1)=1
					else if (fuck<0.6) then
						brain(self_pos(i-1,j+1,maximum_columns),j+1,i-1)=1
					else if (fuck<0.8) then
						brain(self_pos(i,j-1,maximum_columns),j-1,i)=1					
					else
						brain(self_pos(i,j+1,maximum_columns),j+1,i)=1
					end if
					print*,"case3"
				else if ((k==self_pos(i,j,maximum_columns)) .and. (brain(k,j,i)==1) .and. ((i/=1) .or. (i/=maximum_columns)) .and. (j==1)) then
					
					brain(k,j,i)=0
					call RANDOM_NUMBER(fuck)
					if (fuck<0.2) then
						brain(self_pos(i-1,j,maximum_columns),j,i-1)=1
					else if (fuck<0.4) then
						brain(self_pos(i-1,j+1,maximum_columns),j+1,i-1)=1
					else if (fuck<0.6) then
						brain(self_pos(i,j+1,maximum_columns),j+1,i)=1
					else if (fuck<0.8) then
						brain(self_pos(i+1,j,maximum_columns),j,i+1)=1					
					else
						brain(self_pos(i+1,j+1,maximum_columns),j+1,i+1)=1
					end if
					print*,"case4"
				else if ((k==self_pos(i,j,maximum_columns)) .and. (brain(k,j,i)==1) .and. ((i/=1) .or. &
					(i/=maximum_columns)) .and. (j==maximum_columns)) then
					
					brain(k,j,i)=0
					call RANDOM_NUMBER(fuck)
					if (fuck<0.2) then
						brain(self_pos(i-1,j-1,maximum_columns),j-1,i-1)=1
					else if (fuck<0.4) then
						brain(self_pos(i-1,j,maximum_columns),j,i-1)=1
					else if (fuck<0.6) then
						brain(self_pos(i,j-1,maximum_columns),j-1,i)=1
					else if (fuck<0.8) then
						brain(self_pos(i+1,j-1,maximum_columns),j-1,i+1)=1					
					else
						brain(self_pos(i+1,j,maximum_columns),j,i+1)=1
					end if
					print*,"case5"


					!second set of cases - in the corner of the matrix

				else if ((k==self_pos(i,j,maximum_columns)) .and. (brain(k,j,i)==1) .and. (i==1) .and. (j==1)) then
					
					brain(k,j,i)=0
					call RANDOM_NUMBER(fuck)
					if (fuck<1.0/3.0) then
						brain(self_pos(i,j+1,maximum_columns),j+1,i)=1
					else if (fuck<2.0/3.0) then
						brain(self_pos(i+1,j,maximum_columns),j,i+1)=1				
					else
						brain(self_pos(i+1,j+1,maximum_columns),j+1,i+1)=1
					end if
					print*,"case6"
				else if ((k==self_pos(i,j,maximum_columns)) .and. (brain(k,j,i)==1) .and. (i==1) .and. (j==maximum_columns)) then
					
					brain(k,j,i)=0
					call RANDOM_NUMBER(fuck)
					if (fuck<1.0/3.0) then
						brain(self_pos(i,j-1,maximum_columns),j-1,i)=1
					else if (fuck<2.0/3.0) then
						brain(self_pos(i+1,j-1,maximum_columns),j-1,i+1)=1				
					else
						brain(self_pos(i+1,j,maximum_columns),j,i+1)=1
					end if
					print*,"case7"
				else if ((k==self_pos(i,j,maximum_columns)) .and. (brain(k,j,i)==1) .and. (i==maximum_rows) .and. (j==1)) then
					
					brain(k,j,i)=0
					call RANDOM_NUMBER(fuck)
					if (fuck<1.0/3.0) then
						brain(self_pos(i-1,j,maximum_columns),j,i-1)=1
					else if (fuck<2.0/3.0) then
						brain(self_pos(i-1,j+1,maximum_columns),j+1,i-1)=1				
					else
						brain(self_pos(i+1,j+1,maximum_columns),j+1,i+1)=1
					end if
					print*,"case8"
				else if ((k==self_pos(i,j,maximum_columns)) .and. (brain(k,j,i)==1) .and. (i==maximum_rows) .and. (j==maximum_columns)) then
					
					brain(k,j,i)=0
					call RANDOM_NUMBER(fuck)
					if (fuck<1.0/3.0) then
						brain(self_pos(i-1,j-1,maximum_columns),j-1,i-1)=1
					else if (fuck<2.0/3.0) then
						brain(self_pos(i-1,j,maximum_columns),j,i-1)=1				
					else
						brain(self_pos(i,j-1,maximum_columns),j-1,i)=1
					end if
					print*,"case9"
				end if
			end do
		end do
	end do
	print*,"Brain After",thrash+1
	do i=1,size(brain(1,1,:))
		print formatte,brain(self_pos(i,1,maximum_columns),1,i),brain(self_pos(i,2,maximum_columns),2,i),&
			brain(self_pos(i,3,maximum_columns),3,i),brain(self_pos(i,4,maximum_columns),4,i),brain(self_pos(i,5,maximum_columns),5,i)
	end do
	print*," "
end do

end program
