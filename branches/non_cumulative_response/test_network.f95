program test_network
use flesh
implicit none

real,parameter :: pi=4*asin(1./sqrt(2.))
integer :: thrash,i,j,k !i=rows, j=columns
integer,parameter :: maximum_columns=5,maximum_rows=5
integer, dimension(25,maximum_columns,maximum_rows) :: brain
integer, dimension(maximum_columns,maximum_rows) :: brain_freeze !records target of data shift
real :: fuck,me
character(len=16) :: formatte="(I2,I2,I2,I2,I2)"

!make sure the matrix starts off with zeros
do i=1,size(brain(1,1,:))
	do j=1,size(brain(1,:,1))
		brain_freeze(j,i)=0
		do k=1,size(brain(:,1,1))
			brain(k,j,i)=0
		end do
	end do
end do

do thrash=0,15

	!inject ones into matrix
	!here this is done in a sweeping pattern, from left to right, then back to left

	if ((-1)**((thrash/maximum_rows)+1)==-1) then
		brain(mod(thrash,maximum_columns)+1,mod(thrash,maximum_columns)+1,1)=1 !move from left to right
	else
		brain(maximum_columns-mod(thrash,maximum_columns),maximum_columns-mod(thrash,maximum_columns),1)=1 !move from right to left
	end if	
	
	!print the matrix before it gets operated on

	print*,"Brain Before",thrash+1
	do i=1,size(brain(1,1,:))
		print formatte,brain(self_pos(i,1,maximum_columns),1,i),brain(self_pos(i,2,maximum_columns),2,i),&
			brain(self_pos(i,3,maximum_columns),3,i),brain(self_pos(i,4,maximum_columns),4,i),brain(self_pos(i,5,maximum_columns),5,i)
	end do

	!move all the ones

	!first, zero out brain_freeze
	do i=1,size(brain(1,1,:))
		do j=1,size(brain(1,:,1))
			brain_freeze(j,i)=0
		end do
	end do

	do i=1,size(brain(1,1,:))
		do j=1,size(brain(1,:,1))

			!data is in the 3rd address, that corresponds to the position of the row/column, counting left to right, up to down
			k=self_pos(i,j,maximum_columns)

			if (brain(k,j,i)==1) then

				!first case is anything off of the border

				if (((i/=1) .and. (i/=maximum_rows)) .and. ((j/=1) .and. (j/=maximum_columns))) then

					
					call RANDOM_NUMBER(fuck)
					if (fuck<0.125) then
						brain_freeze(j,i)=self_pos(i-1,j-1,maximum_columns)
					else if (fuck<0.25) then
						brain_freeze(j,i)=self_pos(i-1,j,maximum_columns)
					else if (fuck<0.375) then
						brain_freeze(j,i)=self_pos(i-1,j+1,maximum_columns)
					else if (fuck<0.5) then
						brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)					
					else if (fuck<0.625) then
						brain_freeze(j,i)=self_pos(i,j+1,maximum_columns)
					else if (fuck<0.75) then
						brain_freeze(j,i)=self_pos(i+1,j-1,maximum_columns)
					else if (fuck<0.875) then
						brain_freeze(j,i)=self_pos(i+1,j,maximum_columns)
					else
						brain_freeze(j,i)=self_pos(i+1,j+1,maximum_columns)
					end if
					print*,"case1",i,j


				! first set of cases - on the border of the matrix but not in the corner

				else if ((i==1) .and. ((j/=1) .and. (j/=maximum_columns))) then
					
					
					call RANDOM_NUMBER(fuck)
					if (fuck<0.2) then
						brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)
					else if (fuck<0.4) then
						brain_freeze(j,i)=self_pos(i,j+1,maximum_columns)
					else if (fuck<0.6) then
						brain_freeze(j,i)=self_pos(i+1,j-1,maximum_columns)
					else if (fuck<0.8) then
						brain_freeze(j,i)=self_pos(i+1,j,maximum_columns)				
					else
						brain_freeze(j,i)=self_pos(i+1,j+1,maximum_columns)
					end if
					print*,"case2",i,j
				else if ((i==maximum_rows) .and. ((j/=1) .and. (j/=maximum_columns))) then
					
					
					call RANDOM_NUMBER(fuck)
					if (fuck<0.2) then
						brain_freeze(j,i)=self_pos(i-1,j-1,maximum_columns)
					else if (fuck<0.4) then
						brain_freeze(j,i)=self_pos(i-1,j,maximum_columns)
					else if (fuck<0.6) then
						brain_freeze(j,i)=self_pos(i-1,j+1,maximum_columns)
					else if (fuck<0.8) then
						brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)				
					else
						brain_freeze(j,i)=self_pos(i,j+1,maximum_columns)
					end if
					print*,"case3",i,j
				else if (((i/=1) .and. (i/=maximum_columns)) .and. (j==1)) then
					
					
					call RANDOM_NUMBER(fuck)
					if (fuck<0.2) then
						brain_freeze(j,i)=self_pos(i-1,j,maximum_columns)
					else if (fuck<0.4) then
						brain_freeze(j,i)=self_pos(i-1,j+1,maximum_columns)
					else if (fuck<0.6) then
						brain_freeze(j,i)=self_pos(i,j+1,maximum_columns)
					else if (fuck<0.8) then
						brain_freeze(j,i)=self_pos(i+1,j,maximum_columns)					
					else
						brain_freeze(j,i)=self_pos(i+1,j+1,maximum_columns)
					end if
					print*,"case4",i,j
				else if (((i/=1) .and. (i/=maximum_columns)) .and. (j==maximum_columns)) then
					
					
					call RANDOM_NUMBER(fuck)
					if (fuck<0.2) then
						brain_freeze(j,i)=self_pos(i-1,j-1,maximum_columns)
					else if (fuck<0.4) then
						brain_freeze(j,i)=self_pos(i-1,j,maximum_columns)
					else if (fuck<0.6) then
						brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)
					else if (fuck<0.8) then
						brain_freeze(j,i)=self_pos(i+1,j-1,maximum_columns)				
					else
						brain_freeze(j,i)=self_pos(i+1,j,maximum_columns)
					end if
					print*,"case5",i,j


					!second set of cases - in the corner of the matrix

				else if ((i==1) .and. (j==1)) then
					
					
					call RANDOM_NUMBER(fuck)
					if (fuck<1.0/3.0) then
						brain_freeze(j,i)=self_pos(i,j+1,maximum_columns)
					else if (fuck<2.0/3.0) then
						brain_freeze(j,i)=self_pos(i+1,j,maximum_columns)			
					else
						brain_freeze(j,i)=self_pos(i+1,j+1,maximum_columns)
					end if
					print*,"case6",i,j
				else if ((i==1) .and. (j==maximum_columns)) then
					
					
					call RANDOM_NUMBER(fuck)
					if (fuck<1.0/3.0) then
						brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)
					else if (fuck<2.0/3.0) then
						brain_freeze(j,i)=self_pos(i+1,j-1,maximum_columns)				
					else
						brain_freeze(j,i)=self_pos(i+1,j,maximum_columns)
					end if
					print*,"case7",i,j
				else if ((i==maximum_rows) .and. (j==1)) then
					
					
					call RANDOM_NUMBER(fuck)
					if (fuck<1.0/3.0) then
						brain_freeze(j,i)=self_pos(i-1,j,maximum_columns)
					else if (fuck<2.0/3.0) then
						brain_freeze(j,i)=self_pos(i-1,j+1,maximum_columns)			
					else
						brain_freeze(j,i)=self_pos(i,j+1,maximum_columns)
					end if
					print*,"case8",i,j
				else if ((i==maximum_rows) .and. (j==maximum_columns)) then
					
					
					call RANDOM_NUMBER(fuck)
					if (fuck<1.0/3.0) then
						brain_freeze(j,i)=self_pos(i-1,j-1,maximum_columns)
					else if (fuck<2.0/3.0) then
						brain_freeze(j,i)=self_pos(i-1,j,maximum_columns)			
					else
						brain_freeze(j,i)=self_pos(i,j-1,maximum_columns)
					end if
					print*,"case9",i,j

				end if

			end if

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
