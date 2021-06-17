program mastery
implicit none

!this program is a master evolutionary simulator for the network. It makes x copies, runs them for y amount of cycles and then keeps only the x/z best earners

integer,parameter :: simul_total=6,simul_cut_cutoff=2,epoch=10,dream_theatre=100
integer :: simul,account_entries,simul_cut,simul_cut_counter,dream_theatre_count
real,dimension(simul_total) :: account_delta
real :: account_start,account_finish
character(len=7),dimension(simul_total) :: files
character(len=5) :: epoch_name
character(len=4) :: dummy_acc

dream_theatre_count=1

write(epoch_name,"(I0)") epoch

call execute_command_line("rm -r ~/Documents/evolutionary/test*")

!copy the first examples into their folders
do simul=1,simul_total
	write(files(simul),"(A5,I0)") "test_",simul
	call execute_command_line("cp -r ../trunk ~/Documents/evolutionary/"//files(simul))
	call execute_command_line("cd ~/Documents/evolutionary/"//files(simul)//" && ./i_am_in_command.zsh clean "&
		//trim(epoch_name)//" notest")
end do

!read the network performance, finding the difference in teh account value over the epoch
do simul=1,simul_total
	write(files(simul),"(A5,I0)") "test_",simul
	open(unit=1,file="/home/avltbyzn/Documents/evolutionary/"//trim(files(simul))//"/world_in_a_world/account.csv")
	!if this is the first run, skip the first 11 entries
	if (dream_theatre_count==1) then
		do account_entries=1,11
			read(1,*)
		end do
	end if
	!read the first entry
	read(1,*) dummy_acc,account_start
	!skip the rest of the account, to the last entry
	do account_entries=1,epoch
		read(1,*)
	end do
	!read the last entry
	read(1,*) dummy_acc,account_finish
	!calculate the account delta (simple difference)
	account_delta(simul)=account_finish-account_start
	close(1)
end do

!for each test, count how many other tests have made more money over time. If there are more than simul_cut_cutoff, delete the test
do simul=1,simul_total
	simul_cut_counter=0
	do simul_cut=1,simul_total
		if ((simul/=simul_cut) .and. (account_delta(simul)<account_delta(simul_cut))) simul_cut_counter=simul_cut_counter+1
		if (simul_cut_counter>=simul_cut_cutoff) then
			call execute_command_line("rm -r ~/Documents/evolutionary/"//files(simul))
			exit
		end if
	end do
end do
	
do simul=1,simul_cut_cutoff
	call system('ls ~/Documents/evolutionary > fileContents.txt')
	open(31,FILE='fileContents.txt',action="read")
end do

end program
