program mastery
implicit none

integer,parameter :: simul_total=50,simul_cut_cutoff=5,epoch=500
integer :: simul,account_entries,simul_cut,simul_cut_counter
real,dimension(simul_total) :: accounts
character(len=7),dimension(simul_total) :: files
character(len=5) :: epoch_name
character(len=4) :: dummy_acc

write(epoch_name,"(I0)") epoch

call execute_command_line("rm -r ~/Documents/evolutionary/test*")

do simul=1,simul_total
	write(files(simul),"(A5,I0)") "test_",simul
	call execute_command_line("cp -r ../derandomised_progression ~/Documents/evolutionary/"//files(simul))
	call execute_command_line("cd ~/Documents/evolutionary/"//files(simul)//" && ./i_am_in_command.zsh clean "&
		//trim(epoch_name)//" notest")
end do

do simul=1,simul_total
	write(files(simul),"(A5,I0)") "test_",simul
	open(unit=1,file="/home/avltbyzn/Documents/evolutionary/"//trim(files(simul))//"/world_in_a_world/account.csv")
	do account_entries=1,epoch+11
		read(1,*)
	end do
	read(1,*) dummy_acc,accounts(simul)
	close(1)
end do

do simul=1,simul_total
	simul_cut_counter=0
	do simul_cut=1,simul_total
		if ((simul/=simul_cut) .and. (accounts(simul)<accounts(simul_cut))) simul_cut_counter=simul_cut_counter+1
		if (simul_cut_counter>=simul_cut_cutoff) then
			call execute_command_line("rm -r ~/Documents/evolutionary/"//files(simul))
			exit
		end if
	end do
end do
	


end program
