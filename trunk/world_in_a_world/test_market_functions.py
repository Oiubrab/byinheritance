import csv



#csv reader and outputter


#the reader
#if a string representing a filename is inputted into filename_tup, that csv file will be read and returned as a list
#if a tuple of strings representing filenames are added, they will each be entries in a list
def read_csv_dic(filename_tup):
	reader_out=[]
	if type(filename_tup)==str:
		reader = csv.DictReader(open(filename_tup))
		for raw in reader:
			reader_out.append(raw)
	else:
		for filename in filename_tup:
			dic_list=[]
			reader = csv.DictReader(open(filename))
			for raw in reader:
				dic_list.append(raw)
			reader_out += [dic_list]
	return reader_out
	
	
	
	
	
	
	
	
#the writer, writing out to a csv file with dics where the order of filenames must match the order of dics
def write_csv_dic(filename_tup,dic_tup):
	if type(filename_tup)==str:
		with open(filename_tup,'w') as csvfile:
			read_survey_out=csv.DictWriter(csvfile,dic_tup[0].keys())
			read_survey_out.writeheader()
			for dic_insert in dic_tup:
				read_survey_out.writerow(dic_insert)
	else:
		for (filename,dic) in zip(filename_tup,dic_tup):
			with open(filename,'w') as csvfile:
				read_survey_out=csv.DictWriter(csvfile,dic[0].keys())
				read_survey_out.writeheader()
				for dic_insert in dic:
					read_survey_out.writerow(dic_insert)	







					
#this function converts an integer to a binary list, surprising
#note; the binary array is outputted in format left:smallest to right:highest
#the last bit represents pos (1) or neg (0) (a zero is positive)
def integer_to_binary(integer):
	binary=[]
	#special case for 0, 1 and 2
	if integer==0:
		binary=[0]
	elif integer==1:
		binary=[1]
	elif integer==2:
		binary=[0,1]
	#otherwise, compute the binary
	else:
		while abs(integer)>1:
			remainder=int(abs(integer)%2)
			integer=integer/2
			binary = binary + [remainder] 
	
	#if number is a power of two, need to add the last binary
	if len(binary)>1 and sum(binary)==0:
		binary = binary + [1]
			
	#add the pos/neg bit
	if integer<0:
		binary = binary + [0]
	else:
		binary = binary + [1]
		
	return binary
	
	
	
	
	
#this function converts the binary array above into an integer
def binary_to_integer(binary_array):

	#make the integer
	integer=0
	#add up the parts of the binary to make the integer
	for binary,order in zip(binary_array,range(len(binary_array))):
		if order<len(binary_array)-1:
			integer = integer + binary*(2**order)
	
	#use the last part of the binary array as the pos/neg label
	if binary_array[-1] == 0:
		integer = integer*(-1)
	
	return integer
		


#this function converts an integer percentage number into a position on a binary array
def percentage_to_position(percent,size):

	#make the binary list
	binary_list=[0]*size
	
	#find the increment
	increment=200/size
	
	#find the representative position
	for element in range(size):
		if percent<(element+1)*increment-100:
			binary_list[element]=1
			break
			
	return binary_list
