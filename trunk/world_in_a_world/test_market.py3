#kiss my arse 


#this is a script that will stand as a database manipulator, altering the input of the network depending on certain rules

import numpy
import time
import sys
from test_market_functions import *

#this is where the size of the feel array is entered
feel_size=15
#this is where the amplitudes of the market pricing shifts are entered
amplitude_1st=100.0
amplitude_2nd=10.0





#open the market, given the correct choices are made
if len(sys.argv)==2 and (str(sys.argv[1])=='reset' or str(sys.argv[1])=='carryon'):
	if str(sys.argv[1])=='reset':
		markets = read_csv_dic("market_reset.csv")
		account = read_csv_dic("account_reset.csv")
		deicide = open("sight_response_reset.csv","r")
	elif str(sys.argv[1])=='carryon':
		markets = read_csv_dic("market.csv")
		account = read_csv_dic("account.csv")
		deicide = open("sight_response.csv","r")
else:
	print("run application with python3 test_market.py3 start_choice")
	print("Where:")
	print("Start_choice=reset - resets the market")
	print("start_choice=carryon - carries on the market")
	sys.exit()

#open the sight_response.csv array and prepare it to affect it's position in the market
deicide_array=csv.reader(deicide)
deicide_array_list=list(deicide_array)
deicide_array_list_numbers=[int(number) for number in deicide_array_list[0]]
#print(deicide_array_list_numbers)
	
#print(markets)
#print(account)





#data initialisation

#controlling variable for siunosoidal market movement 
#use the time stamp from the last account entry
t_start = float(account[-1]["time"])
t_end = time.time()
timer = t_end - t_start


#numerise all the stock prices and units
for stock in markets:
	stock["stock_number"]=int(stock["stock_number"])
	stock["stock_price"]=float(stock["stock_price"])
	stock["units_owned"]=int(stock["units_owned"])

#numerise all the account values and times
for entry in account:
	entry["account_value"]=float(entry["account_value"])
	entry["time"]=float(entry["time"])






#choices
#deicide[0:2] is stock selector, deicide[3:5] is number of units, deicide[6] is buy/sell (pos or neg units)
#0 is sell, 1 is buy

#find the stock numerical identifier
stock_selection = binary_to_integer(deicide_array_list_numbers[0:3] + [1])
#interpret the number of units and buy/sell choice
deicide_length = len(deicide_array_list_numbers)
units = binary_to_integer(deicide_array_list_numbers[3:deicide_length-1])


#buy/sell controller
#first, find the most recent line in the ledger
old_time=0.0
for entry in account:
	if entry["time"]>old_time:
		old_time=entry["time"]
		last_entry=entry
		
#buy/sell algorithm
#increase/decrease units owned of the selected stock by the number of units selected
#initialisation of cost at 0 ensures invalid choices by the network are ignored 
cost=0
for stock in markets:
	# second condition (stock+units>-1) ensures network cannot sell more than it has
	#third condition (last_entry-units*stock>0.0) ensures network cannot go into debt 
	if stock["stock_number"]==stock_selection and stock["units_owned"]+units>-1 and last_entry["account_value"]-units*stock["stock_price"]>0.0:
		#buying (positive cost) and selling (negative cost)
		stock["units_owned"] += units
		cost = units*stock["stock_price"]

		
#subtract from account
#add a new entry to the account that has this subtracted value
this_entry = {"account":"test","account_value":last_entry["account_value"]-cost,"time":t_end}
account = account + [this_entry]


#temporary account drain - pass drained funds to a holding account
trigger=True
if trigger==True and account[-1]["account_value"]>23000.0:
	to_add = account[-1]["account_value"]-3000.0
	account[-1]["account_value"]=3000.0
	summer = open("holding_sum.txt")
	the_sum=float(summer.read())
	the_sum+=to_add
	winter = open("holding_sum.txt","w")
	winter.write(str(the_sum))
	

#write this dic to a csv file
write_csv_dic("account.csv",account)

#compute weighted gradient of earnings and place it in a binary array
weighted_gradient_percentage = int(((this_entry["account_value"] - last_entry["account_value"])/abs(this_entry["account_value"]))*100.0)
#limit growth/decay to the last account volume for now
if weighted_gradient_percentage>100.0:
	weighted_gradient_percentage=100.0
elif weighted_gradient_percentage<-100.0:
	weighted_gradient_percentage=-100.0
	

	
#prepare binary array 
weighted_gradient_percentage_binary = integer_to_binary(weighted_gradient_percentage)

#if the binary array is smaller than the feel array it's going to, resize the array
if len(weighted_gradient_percentage_binary)<feel_size:
	last_digit=weighted_gradient_percentage_binary[-1]
	weighted_gradient_percentage_binary[-1]=0
	difference_lengths=feel_size-len(weighted_gradient_percentage_binary)
	#pad out thelist with zeros
	for x in range(difference_lengths-1):
		weighted_gradient_percentage_binary = weighted_gradient_percentage_binary + [0]
	#restore the pos/neg trigger at the end
	weighted_gradient_percentage_binary = weighted_gradient_percentage_binary + [last_digit]
		
	

#and this is where the resulting array is written into feel.csv
feel = numpy.array(weighted_gradient_percentage_binary)
wtr = csv.writer(open ('feel.csv', 'w'), delimiter=',', lineterminator='\n')
wtr.writerow(feel)







#alter the stock to replicate a changing market
for stock in markets:
	#some stocks will rise, some will fall
	if stock["stock_number"]%2==1:
		change = float(stock["stock_price"])+(amplitude_1st*numpy.sin(t_end))#+(amplitude_2nd*numpy.sin(t_end*0.01))
	else:
		change = float(stock["stock_price"])+(amplitude_1st*numpy.sin(t_end))-(amplitude_2nd*numpy.sin(t_end*0.01))
	#print(change)
	#give stocks a floor and a ceiling
	stock["stock_price"] = change
	if stock["stock_price"]<10.0:
		stock["stock_price"]=10.0
	elif stock["stock_price"]>1000.0:
		stock["stock_price"]=1000.0
	
write_csv_dic("market.csv",markets)








#this is where the market is interpreted and folded into the sight of the network
#make a numerical list for the stocks
#place binary representation, followed by listing number, into the pit
for exam in markets:

	#make binary lists out of the stock and stock_number
	stock_binary = integer_to_binary(int(exam["stock_price"]))
	stock_number_binary = integer_to_binary(exam['stock_number'])
	
	# ensure stocks binaries are deposited in 15 bit chunks by padding
	#place pos neg label at the end
	stock_height=15
	if len(stock_binary)==2:
		stock_binary = [stock_binary[0]] + [0 for num in range(stock_height-len(stock_binary))] + [stock_binary[-1]]
	elif len(stock_binary)<stock_height:
		stock_binary = stock_binary[0:-1] + [0 for num in range(stock_height-len(stock_binary))] + [stock_binary[-1]]

		
	# ensure stock_number_binary is deposited in 3 bit chunks by padding 
	stock_number_height=3
	if len(stock_number_binary)==2:
		stock_number_binary = [stock_number_binary[0]] + [0 for num in range(stock_number_height-len(stock_number_binary))] + [stock_number_binary[-1]]
	elif len(stock_number_binary)<stock_number_height:
		stock_number_binary = stock_number_binary[0:-1] + [0 for num in range(stock_number_height-len(stock_number_binary))] + [stock_number_binary[-1]]
	
	#and this is where the resulting array is written into sight.csv
	#add an extra zero to make list length an odd number
	sight = numpy.array(stock_number_binary + stock_binary + [0])
	#print(len(sight))
	#print(stock_number_binary[0:-2] , [0 for num in range(stock_number_height-len(stock_number_binary))] , [stock_number_binary[-1]])
	#print(sight,stock_binary,stock_number_binary,exam['stock_identifier'],int(exam["stock_price"]),exam['stock_number'])
	wtr = csv.writer(open ('sight_'+exam['stock_identifier']+'.csv', 'w'), delimiter=',', lineterminator='\n')
	wtr.writerow(sight)



	



print(markets)
print(this_entry)
