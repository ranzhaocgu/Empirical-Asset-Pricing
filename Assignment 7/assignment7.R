setwd('C:\\Users\\ranzhao\\Documents\\Empirical-Asset-Pricing\\Assignment 7')
setwd('D:\\PhD FE\\Empirical-Asset-Pricing\\Assignment 7')
setwd('D:\\Empirical-Asset-Pricing\\Assignment 7')

require(data.table)
option.data = fread('VIXoptions.csv', header = T, sep = ',')
implied.data = fread('VIXoptionsStd.csv', header = T, sep = ',',select=c("date","days","forward_price"))
implied.data = unique(implied.data)


