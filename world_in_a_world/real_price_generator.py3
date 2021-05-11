import numpy
import time
import sys
import random
from test_market_functions import *

step=int(sys.argv[1])-1
total_steps=int(sys.argv[2])

stock_ranges = [read_csv_dic("stocks/BRN.csv"),read_csv_dic("stocks/SE1.csv"),read_csv_dic("stocks/FMG.csv")]
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
	
	if total_steps<len(stock_range)*2:
		if step%2==0:
			market["stock_price"]=stock_range[int(step/2)]['Low']
		else:
			market["stock_price"]=stock_range[int(step/2)]['High']
			
	else:
		inbetween=total_steps/len(stock_range)
		interstep=int(step/inbetween)
		rander=random.uniform(float(stock_range[interstep]['Low']),float(stock_range[interstep]['High']))
		rander=truncate(rander,3)
		market["stock_price"]=rander
		
	if float(market["stock_price"])<0.001:
		market["stock_price"]=0.001
		
write_csv_dic("market.csv",markets)

