setwd('C:\\Users\\ranzhao\\Documents\\Empirical Asset Pricing\\Assignment 1')
setwd('D:\\PhD FE\\Empirical-Asset-Pricing\\Assignment 1')

# Data loading
require(ggplot2)
spx_index_values = read.csv('spx_index_values.csv', header = TRUE)
t_bill_3M_values = read.csv('TB3MS.csv', header = TRUE)
plot(as.Date(as.character(t_bill_3M_values$DATE), "%m/%d/%Y"), t_bill_3M_values$TB3MS, type='l', 
     main='3-Month Treasury Bill, from 1954 to 2015',
     xlab='year', ylab='rate (in percentage)')
# add the moving average of the rates to the plot, ggplot?


# Data segments
ir_full = t_bill_3M_values
ir_1954_1975 = t_bill_3M_values[
  as.Date(as.character(t_bill_3M_values$DATE), "%m/%d/%Y") >= as.Date('1954-01-01') &
  as.Date(as.character(t_bill_3M_values$DATE), "%m/%d/%Y") <= as.Date('1975-12-31'), ]
ir_1976_1981 = t_bill_3M_values[
  as.Date(as.character(t_bill_3M_values$DATE), "%m/%d/%Y") >= as.Date('1976-01-01') &
    as.Date(as.character(t_bill_3M_values$DATE), "%m/%d/%Y") <= as.Date('1981-12-31'), ]
ir_1982_2005 = t_bill_3M_values[
  as.Date(as.character(t_bill_3M_values$DATE), "%m/%d/%Y") >= as.Date('1982-01-01') &
    as.Date(as.character(t_bill_3M_values$DATE), "%m/%d/%Y") <= as.Date('2005-12-31'), ]
ir_2006_2015 = t_bill_3M_values[
  as.Date(as.character(t_bill_3M_values$DATE), "%m/%d/%Y") >= as.Date('2006-01-01') &
    as.Date(as.character(t_bill_3M_values$DATE), "%m/%d/%Y") <= as.Date('2015-12-31'), ]