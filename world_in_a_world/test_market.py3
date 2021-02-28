#kiss my arse 


#this is a script that will stand as a database manipulator, altering the input of the network depending on certain rules

import numpy
import time
import sys
from test_market_functions import *

if str(sys.argv[1])=='reset':
	markets = read_csv_dic("market_reset.csv")
elif str(sys.argv[1])=='carryon':
	markets = read_csv_dic("market.csv")
	
print(markets)

t_start = time.time()

#numerise all the stock prices and units
for stock in markets:
	number = stock["stock_price"]
	stock["stock_price"]=float(stock["stock_price"])
	stock["units_owned"]=int(stock["units_owned"])

#alter the stock to replicate a changing market
for stock in markets:
	change = float(stock["stock_price"])*numpy.sin(t_start)
	print(change)
	stock["stock_price"] = change+stock["stock_price"]
	if stock["stock_price"]<0.0:
		stock["stock_price"]=10.0
	
write_csv_dic("market.csv",markets)

print(markets)
