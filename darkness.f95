module darkness
use welcome_to_dying
!this module contains all the testing functions
contains

!delays in microsecond precision
subroutine delay(time)

	real :: time, starter,finisher
	logical :: past=.false.
	
	call cpu_time(starter)
	do while (past .eqv. .false.)
		call cpu_time(finisher)
		if (finisher>starter+time) then
			past=.true.
		end if
	end do
	
	past=.false.
	
end subroutine delay




!prints the network and input/output arrays to a text file that is numerically labelled
subroutine print_network(imager,moves,vision,vision_socket,response,response_socket,brain,blood,starter,finisher)

	integer,dimension(*) :: brain(:,:,:),vision(:),response(:)
	real,optional,dimension(*) :: blood(:,:)
	
	real,optional :: starter,finisher
	real :: t_sec, total_time
	integer :: t_hr, t_min	
	
	integer :: row_counter,column_counter,rows,columns,info_ports,blood_rows,blood_columns,vision_columns,response_columns,imager
	integer,intent(in) :: vision_socket,response_socket,moves
	!printing
	integer,parameter :: individual_width=2, individual_blood=6, separation_space=10
	character(len=individual_width) :: data_cha
	character(len=individual_blood) :: blood_cha
	character(len=12) :: individual_width_cha,separation_cha,individual_blood_cha
	character(len=17) :: width,blood_width,empty_width
	character(len=20) :: tester
	character(len=:),allocatable :: print_row
	
	
	!Who is your daddy and what does he do?
	write(tester,"(A8,I0,A4)") "test_log",imager,".txt"	
	open(unit=imager,file=tester,access="APPEND")

	write(imager,*)"By Inheritance"
	write(imager,'(A15,I0)')"Brain moves: ",moves
	
	!establish network dimensions
	rows=size(brain(1,1,:)); columns=size(brain(1,:,1)); info_ports=size(brain(:,1,1))
	!if blood is present, set ther size, otherwise the size is zero
	if (present(blood)) then
		blood_rows=size(blood(1,:)); blood_columns=size(blood(:,1))
	else 
		blood_rows=0; blood_columns=0
	end if	
	!and set the com arrays
	vision_columns=size(vision); response_columns=size(response)
	
	!allocate the printing row with enough space to fit both brain and blood if blood is present
	if (present(blood)) then
		allocate(character((columns*individual_width)+(blood_columns*individual_blood)+2+separation_space) :: print_row) !allocate the printing variable
	else
		allocate(character((columns*individual_width)+2) :: print_row) !allocate the printing variable	
	end if
	
	!set the width to print for each datum
	write(individual_width_cha,*)individual_width
	write(individual_blood_cha,*)individual_blood
	!width for brain/blood
	width="(I"//trim(individual_width_cha)//")"
	empty_width="(A"//trim(individual_width_cha)//")"
	blood_width="(F"//trim(individual_blood_cha)//".2)"

	!print the vision array first
	do column_counter=1,columns
		!print*,plugin(column_counter,vision_socket,vision_columns,"array"),column_counter
		!set up printer to print vision array in the correct columns
		if ((column_counter>=plugin(1,vision_socket,vision_columns,"brain")) .and. &
			(column_counter<=plugin(vision_columns,vision_socket,vision_columns,"brain"))) then
			write(data_cha,width)vision(plugin(column_counter,vision_socket,vision_columns,"array"))
		else
			write(data_cha,empty_width)"  "
		end if
		print_row(column_counter*individual_width-(individual_width-1):column_counter*individual_width)=data_cha
	end do
	write(imager,*)print_row(1:individual_width*columns)
	write(imager,*)" "
	print_row(:)="  "

	!the main brain printing loop
	do row_counter=1,rows
		!this loop now handles printing both the brain and the blood networks, hence the columns*2
		!the columns+1 position is empty and creates a space between the two networks
		do column_counter=1,columns+blood_columns+1
			!add brain numbers to the first half of the row
			if (column_counter<=columns) then
				write(data_cha,width)brain(info_ports,column_counter,row_counter)
				print_row(column_counter*individual_width-(individual_width-1):column_counter*individual_width)=data_cha
			!print the brain and blood networks beside eachother if blood is passed in
			else if (present(blood)) then
				!create a break for the two networks
				if (column_counter==columns+1) then
					print_row(column_counter*individual_width-(individual_width-1):&
						column_counter*individual_width-(individual_width-1)+separation_space)="  "
				!and blood numbers to the second half
				else
					write(blood_cha,blood_width)blood(column_counter-(columns+1),row_counter)
					!from: brain columns + separation + blood column number * blood column width - (blood column width + 1)
					!to: brain columns + separation + blood column number * blood column width
					print_row(columns*individual_width+separation_space+(column_counter-(columns+1))*&
						individual_blood-(individual_blood-1):columns*individual_width+separation_space+&
						(column_counter-(columns+1))*individual_blood)=blood_cha
				end if
			end if
		end do
		write(imager,*)print_row
	end do
	write(imager,*)" "
	print_row(:)="  "

	!print the response and equivalent blood arrays last
	do column_counter=1,columns+blood_columns+1
		!add response numbers to the first half of the row
		if (column_counter<=columns) then
			!place them where the socket is
			if ((column_counter>=plugin(1,response_socket,response_columns,"brain")) .and. &
				(column_counter<=plugin(response_columns,response_socket,response_columns,"brain"))) then
				write(data_cha,width)response(plugin(column_counter,response_socket,response_columns,"array"))
			else
				write(data_cha,empty_width)"  "
			end if
		print_row(column_counter*individual_width-(individual_width-1):column_counter*individual_width)=data_cha
		!print the brain and blood networks beside eachother if blood is passed in
		else if (present(blood)) then
			if (column_counter==columns+1) then
				print_row(column_counter*individual_width-(individual_width-1):&
					column_counter*individual_width-(individual_width-1)+separation_space)="  "
			!and blood numbers to the second half
			else
				write(blood_cha,blood_width)blood(column_counter-(columns+1),blood_rows)
				!from: brain columns + separation + blood column number * blood column width - (blood column width + 1)
				!to: brain columns + separation + blood column number * blood column width
				print_row(columns*individual_width+separation_space+(column_counter-(columns+1))*&
					individual_blood-(individual_blood-1):columns*individual_width+separation_space+&
					(column_counter-(columns+1))*individual_blood)=blood_cha
			end if
		end if
	end do
	write(imager,*)print_row
	write(imager,*)" "
	
	if ((present(starter)) .and. (present(finisher))) then
		total_time=finisher-starter
		t_hr = floor(total_time/3600)
		t_min = (total_time-t_hr*3600)/60
		t_sec = total_time-t_hr*3600-t_min*60
		write(imager,'(A24,I2,A5,I2,A7,F7.4,A4)')"simulator time elapsed =",t_hr,' hrs, ',t_min,' mins, ',t_sec,' sec'
	end if	
	
	!close this shit down
	close(imager)
	
end subroutine print_network




end module
