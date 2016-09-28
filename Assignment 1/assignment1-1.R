setwd('C:\\Users\\ranzhao\\Documents\\Empirical Asset Pricing\\Assignment 1')
setwd('D:\\PhD FE\\Empirical-Asset-Pricing\\Assignment 1')

# Data loading
require(ggplot2)
require(stats4)
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

# Full time period
rate_data = t_bill_3M_values[,2]/12

LL.dev = function(input_data){
  phi = input_data[1]
  X.bar = input_data[2]
  sigma.sq = input_data[3]
  x0 = rate_data[1]
  N = length(rate_data)
  X.bar.dev = -(1-phi^2)*(X.bar-x0)/(2*sigma.sq) + (1-phi)/sigma.sq*sum(rate_data[2:(length(rate_data))] - X.bar*(1 - phi)-phi*rate_data[1:(length(rate_data)-1)])
  phi.dev = -phi/(1-phi^2) + phi*(x0-X.bar)^2/sigma.sq - 1/sigma.sq*sum((rate_data[2:(length(rate_data))] - X.bar*(1 - phi)-phi*rate_data[1:(length(rate_data)-1)])*(X.bar-rate_data[1:(length(rate_data)-1)]))
  sigma.dev = -(N+1)/(2*sigma.sq) + (1-phi^2)*(x0-X.bar)^2/(2*sigma.sq^2) + 1/(2*sigma.sq^2) *sum((rate_data[2:(length(rate_data))] - X.bar*(1 - phi)-phi*rate_data[1:(length(rate_data)-1)])^2)
  obj = X.bar.dev^2 + phi.dev^2 + sigma.dev^2
  return(obj)
}
output = optim(c(1/1.1,mean(rate_data),var(rate_data)), LL.dev, method = "L-BFGS-B", lower = c(-0.99,mean(rate_data)/10,var(rate_data)/10), upper = c(0.99, mean(rate_data)*10,var(rate_data)*10))
para.full = output$par

# time period 1
rate_data = ir_1954_1975[,2] / 12
output = optim(c(1/1.1,mean(rate_data),var(rate_data)), LL.dev, method = "L-BFGS-B", lower = c(-0.99,mean(rate_data)/10,var(rate_data)/10), upper = c(0.99, mean(rate_data)*10,var(rate_data)*10))
para.tp1 = output$par

# time period 2
rate_data = ir_1976_1981[,2] / 12
output = optim(c(1/1.1,mean(rate_data),var(rate_data)), LL.dev, method = "L-BFGS-B", lower = c(-0.99,mean(rate_data)/10,var(rate_data)/10), upper = c(0.99, mean(rate_data)*10,var(rate_data)*10))
para.tp2 = output$par

# time period 3
rate_data = ir_1982_2005[,2] / 12
output = optim(c(1/1.1,mean(rate_data),var(rate_data)), LL.dev, method = "L-BFGS-B", lower = c(-0.99,mean(rate_data)/10,var(rate_data)/10), upper = c(0.99, mean(rate_data)*10,var(rate_data)*10))
para.tp3 = output$par

# time period 4
rate_data = ir_2006_2015[,2] / 12
output = optim(c(1/1.3,mean(rate_data),var(rate_data)), LL.dev, method = "L-BFGS-B", lower = c(-0.99,mean(rate_data)/10,var(rate_data)/10), upper = c(0.99, mean(rate_data)*10,var(rate_data)*10))
para.tp4 = output$par

cbind(para.full, para.tp1, para.tp2, para.tp3, para.tp4)
