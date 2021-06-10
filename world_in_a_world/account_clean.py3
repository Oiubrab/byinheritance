from test_market_functions import *

account = read_csv_dic("account.csv")

#archive function to write
write_csv_dic("account_archive.csv",account)

new_account = [account[-3],account[-2],account[-1]]

write_csv_dic("account.csv",new_account)
