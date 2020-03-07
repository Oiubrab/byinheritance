program test_network
use flesh
implicit none

real,parameter :: pi=4*asin(1./sqrt(2.))
integer :: thrash,i,l,m,j,k,h,g,tester_conflict,tester_multi !i,l=rows, j,m=columns, k=self_pos, g=multi_pos, h=conflict numerator, tester_conflict=true/false conflicts found, tester_multi= true; data donor chosen
integer,parameter :: maximum_columns=5,maximum_rows=5
integer, dimension(25,maximum_columns,maximum_rows) :: brain
integer,dimension(9) :: multi_target
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

	!all the transitions are first recorded in the brain_freeze matrix

	!first, zero out brain_freeze
	do i=1,size(brain(1,1,:))
		do j=1,size(brain(1,:,1))
			brain_freeze(j,i)=0
		end do
	end do

	do i=1,size(brain(1,1,:))
		do j=1,size(brain(1,:,1))

			call neuron_fire(brain,brain_freeze,j,i,maximum_columns,maximum_rows)

		end do
	end do

	!then conflicts are checked for
	!for each position j,i, conflicts are tested throughout the matrix at m,l
	tester_conflict=1
	do while (tester_conflict==1)
		tester_conflict=0

		do i=1,size(brain_freeze(1,:))
			do j=1,size(brain_freeze(:,1))

				!only check non-zero entries
				if (brain_freeze(j,i)/=0) then
					multi_target=(/0,0,0,0,0,0,0,0,0/)

					do l=1,size(brain_freeze(1,:))
						do m=1,size(brain_freeze(:,1))
							
							!store all the target values in the multi_target array
							if ((brain_freeze(j,i)==brain_freeze(m,l)) .and. (multi_target(1)==0)) then
								multi_target(1)=brain_freeze(j,i)
								multi_target(2)=self_pos(i,j,maximum_columns)
								multi_target(3)=self_pos(l,m,maximum_columns)
								tester_conflict=1
								g=3
							else if (brain_freeze(j,i)==brain_freeze(m,l)) then
								do while (multi_target(g)>0)
									g=g+1
								end do
								multi_target(g)=self_pos(l,m,maximum_columns)
							end if

						end do
					end do

				end if
				
				if (tester_conflict==1) then
					!find the lucky neuron that gets to blow it's load
					call RANDOM_NUMBER(fuck)
					h=0
					tester_multi=0
					do while (tester_multi==0)
						h=h+1
						!this determines which neuron gets to input data into the target
						if (fuck<(real(h)/real(g-1))) then
							tester_multi=1
						end if
					end do 	

					!the unlucky neurons now must find another target
					g=2
					do while (multi_target(g)>0)
						if (g/=h+1) then
							call neuron_fire(brain,brain_freeze,point_pos_matrix(multi_target(g),maximum_columns,"column"),&
								point_pos_matrix(multi_target(g),maximum_columns,"row"),maximum_columns,maximum_rows)
						end if
					end do

				end if

			end do
		end do

	end do

	!finally, transact the recorded transitions in brain_freeze
	do i=1,size(brain(1,1,:))
		do j=1,size(brain(1,:,1))
			brain(self_pos(i,j,maximum_columns),j,i)=0
			brain(brain_freeze(j,i),point_pos_matrix(brain_freeze(j,i),maximum_columns,"column"),&
							point_pos_matrix(brain_freeze(j,i),maximum_columns,"row"))=1
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
