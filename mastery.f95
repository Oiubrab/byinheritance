program mastery
implicit none

!this program is a master evolutionary simulator for the network. It makes x copies, runs them for y amount of cycles and then keeps only the x/z best earners
!at this point, the network is really becoming a simulacrum of biology
!note: simul_cut_cutoff must be an integer multiple of simul_total 

integer,parameter :: simul_total=30,simul_cut_cutoff=3,epoch=100,dream_theatre=30
integer :: simul,account_entries,simul_cut,simul_cut_counter,regen,simul_check_counter,dream,simul_check
real,dimension(2,simul_total) :: account_delta !(1,x) is account difference, (2,x) is simul
real,dimension(2) :: account_sort,account_swap
real :: account_start,account_finish
character(len=7),dimension(simul_total) :: files
character(len=7) :: file_survive
character(len=5) :: epoch_name
character(len=4) :: dummy_acc

write(epoch_name,"(I0)") epoch

call execute_command_line("rm -r ~/Documents/evolutionary/test*")
call execute_command_line("rm -r ~/Documents/evolutionary/new_song")

!label
print "(A7,I0,A4,I0)","Dream: ",0," of:",dream_theatre

!copy the first examples into their folders
do simul=1,simul_total
	print "(A17,I0,A4,I0)","Running Network: ",simul," of:",simul_total
	write(files(simul),"(A5,I0)") "test_",simul
	call execute_command_line("cp -r ../trunk ~/Documents/evolutionary/"//files(simul))
	call execute_command_line("cd ~/Documents/evolutionary/"//files(simul)//" && ./i_am_in_command.zsh clean "&
		//trim(epoch_name)//" notest noprint")
end do






!obey your master
do dream=1,dream_theatre

	!label
	print "(A7,I0,A4,I0)","Dream: ",dream," of:",dream_theatre

	!read the network performance, finding the difference in teh account value over the epoch
	do simul=1,simul_total
		write(files(simul),"(A5,I0)") "test_",simul
		open(unit=1,file="/home/avltbyzn/Documents/evolutionary/"//trim(files(simul))//"/world_in_a_world/account.csv")
		!if this is the first run, skip the first 11 entries
		do account_entries=1,11
			read(1,*)
		end do
		!read the first entry
		read(1,*) dummy_acc,account_start
		!skip the rest of the account, to the last entry
		do account_entries=1,epoch-1
			read(1,*)
		end do
		!read the last entry
		read(1,*) dummy_acc,account_finish
		!calculate the account delta (simple difference)
		account_delta(1,simul)=account_finish-account_start
		account_delta(2,simul)=float(simul)+0.01
		close(1)
	end do

	!for each test, count how many other tests have made more money over time. If there are more than simul_cut_cutoff, delete the test
	!sort the list
	print*," "
	do simul=1,simul_total-1
		account_sort=account_delta(:,simul)
		do simul_check=simul+1,simul_total
			if (account_delta(1,simul_check)>account_sort(1)) then
				account_delta(:,simul)=account_delta(:,simul_check)
				account_delta(:,simul_check)=account_sort
				account_sort=account_delta(:,simul)
			end if
		end do
	end do
	!now, remove all the networks undr the simul_cut_cutoff threshold
	do simul=simul_cut_cutoff+1,simul_total
		call execute_command_line("rm -r ~/Documents/evolutionary/"//files(int(account_delta(2,simul))))
	end do
			
				
	!save a list of the tests that survived
	call execute_command_line('ls ~/Documents/evolutionary > fileContents.txt')
	call execute_command_line("mkdir ~/Documents/evolutionary/new_song")


	!use the surviving networks to re-populate the evolutionary folder	
	open(31,FILE='fileContents.txt',action="read")
	do simul=1,simul_cut_cutoff
		read(31,*) file_survive
		do regen=1,simul_total/simul_cut_cutoff
			call execute_command_line("cp -r ~/Documents/evolutionary/"//trim(file_survive)//&
				" ~/Documents/evolutionary/new_song/"//files((simul_total/simul_cut_cutoff)*simul-(regen-1)))
		end do
	end do
	close(31)

	!move the tests in new song back into eveloutionary folder
	call execute_command_line("rm -r ~/Documents/evolutionary/test*")
	call execute_command_line("cp -r ~/Documents/evolutionary/new_song/test* ~/Documents/evolutionary/")
	call execute_command_line("rm -r ~/Documents/evolutionary/new_song")

	!run the copied networks again
	do simul=1,simul_total
		!label
		print "(A17,I0,A4,I0)","Running Network: ",simul," of:",simul_total
		call execute_command_line("cd ~/Documents/evolutionary/"//files(simul)//" && ./i_am_in_command.zsh noclean "&
			//trim(epoch_name)//" notest noprint")
	end do

end do

end program
