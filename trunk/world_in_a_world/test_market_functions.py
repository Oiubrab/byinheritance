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
