#kiss my arse 


#this is a script that will stand as a database manipulator, altering the input of the network depending on certain rules

import numpy
import time
import sys
from test_market_functions import *



#open the sight_response.csv array and prepare it to affect it's position in the market
deicide = open("sight_response.csv","r")
deicide_array=csv.reader(deicide)
deicide_array_list=list(deicide_array)
deicide_array_list_numbers=[int(number) for number in deicide_array_list[0]]
#print(deicide_array_list_numbers)




#open the market, given the correct choices are made
if len(sys.argv)==2 and (str(sys.argv[1])=='reset' or str(sys.argv[1])=='carryon'):
	if str(sys.argv[1])=='reset':
		markets = read_csv_dic("market_reset.csv")
		account = read_csv_dic("account_reset.csv")
	elif str(sys.argv[1])=='carryon':
		markets = read_csv_dic("market.csv")
		account = read_csv_dic("account.csv")
else:
	print("run application with python3 test_market.py3 start_choice")
	print("Where:")
	print("Start_choice=reset - resets the market")
	print("start_choice=carryon - carries on the market")
	sys.exit()
	
print(markets)
print(account)





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
units = binary_to_integer(deicide_array_list_numbers[3:7])
print(deicide_array_list_numbers,deicide_array_list_numbers[0:3],deicide_array_list_numbers[3:7])
print(stock_selection)
print(units)


#buy/sell controller
#first, find the most recent line in the ledger
old_time=0.0
for entry in account:
	if entry["time"]>old_time:
		old_time=entry["time"]
		last_entry=entry
		
#buy/sell algorithm
#increase/decrease units owned of the selected stock by the number of units selected
for stock in markets:
	if stock["stock_number"]==stock_selection:
		#buying (positive cost) and selling (negative cost)
		stock["units_owned"] += units
		cost = units*stock["stock_price"]

		
#subtract from account
#add a new entry to the account that has this subtracted value
this_entry = last_entry
this_entry["account_value"] -= cost
this_entry["time"] = t_end
account = account + [this_entry]

#compute weighted gradient of earnings and place it in a binary array
weighted_gradient_percentage = int(((this_entry["account_value"] - last_entry["account_value"])/this_entry["account_value"])*100)
weighted_gradient_percentage_binary = integer_to_binary(weighted_gradient_percentage)

#and this is where the resulting array is written into feel.csv
feel = numpy.array(weighted_gradient_percentage_binary)
wtr = csv.writer(open ('feel.csv', 'w'), delimiter=',', lineterminator='\n')
wtr.writerow(feel)







#alter the stock to replicate a changing market
for stock in markets:
	change = float(stock["stock_price"])*numpy.sin(timer)
	#print(change)
	stock["stock_price"] = change+stock["stock_price"]
	if stock["stock_price"]<10.0:
		stock["stock_price"]=10.0
	
write_csv_dic("market.csv",markets)








#this is where the market is interpreted and folded into the sight of the network
#make a numerical list for the stocks
stock_list=range(len(markets))
sight_list=[]
#place binary representation, followed by listing number, into the pit
for exam,stock_number in zip(markets,stock_list):

	#make binary lists out of the stock and stock_number
	stock_binary = integer_to_binary(int(exam["stock_price"]))
	stock_number_binary = integer_to_binary(stock_number)
	
	# ensure stocks binaries are deposited in 15 bit chunks by padding
	stock_height=15
	if len(stock_binary)<stock_height:
		stock_binary = [0 for num in range(stock_height-len(stock_binary))] + stock_binary

		
	# ensure stock_number_binary is deposited in 3 bit chunks by padding 
	stock_number_height=3
	if len(stock_number_binary)<stock_number_height:
		stock_number_binary = [0 for num in range(stock_number_height-len(stock_number_binary))] + stock_number_binary

		
	sight_list = sight_list + stock_number_binary + stock_binary
	#exam["stock_identifier"]






#add a 0 for buffer for even lists
if int(len(sight_list)%2)==0:
	sight_list = sight_list +[0]






#and this is where the resulting array is written into sight.csv
sight = numpy.array(sight_list)
wtr = csv.writer(open ('sight.csv', 'w'), delimiter=',', lineterminator='\n')
wtr.writerow(sight)



print(markets)
print(account)
