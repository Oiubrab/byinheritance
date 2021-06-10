#This places real data from stocks in the stock market
#if more data points are needed than are available, random values are generated between the highs and lows of each day
import numpy
import time
import sys
import random
from test_market_functions import *

step=int(sys.argv[1])-1
total_steps=int(sys.argv[2])

stock_ranges = [read_csv_dic("stocks/SE1.csv"),read_csv_dic("stocks/ADV.csv"),read_csv_dic("stocks/SBR.csv")]
if step==0:
	markets=[{'stock_identifier': 'SE1', 'stock_number': 1, 'stock_price': 0.0, 'units_owned': 0}, {'stock_identifier': 'ADV', 'stock_number': 2, 'stock_price': 0.0, 'units_owned': 0}, {'stock_identifier': 'SBR', 'stock_number': 3, 'stock_price': 0.0, 'units_owned': 0}]
else:
	markets=read_csv_dic("market.csv")


random.seed()

def truncate(f, n):
    '''Truncates/pads a float f to n decimal places without rounding'''
    s = '{}'.format(f)
    if 'e' in s or 'E' in s:
        return '{0:.{1}f}'.format(f, n)
    i, p, d = s.partition('.')
    return '.'.join([i, (d+'0'*n)[:n]])

for stock_range,market in zip(stock_ranges,markets):
	
	#for the case where there is enough data points
	if total_steps<len(stock_range)*2:
		if step%2==0:
			market["stock_price"]=stock_range[int(step/2)]['Low']
		else:
			market["stock_price"]=stock_range[int(step/2)]['High']
		
	#otherwise, if there aren't enough data points		
	else:
		inbetween=total_steps/len(stock_range)
		interstep=int(step/inbetween)
		rander=random.uniform(float(stock_range[interstep]['Low']),float(stock_range[interstep]['High']))
		rander=truncate(rander,3)
		market["stock_price"]=rander
		
	if float(market["stock_price"])<0.001:
		market["stock_price"]=0.001
		
write_csv_dic("market.csv",markets)

