setwd('C:\\Users\\ranzhao\\Documents\\Empirical-Asset-Pricing\\Assignment 1')
setwd('D:\\PhD FE\\Empirical-Asset-Pricing\\Assignment 1')


# Data loading
#require(ggplot2)
spx_index_values = read.csv('spx_index_values.csv', header = TRUE)
plot(as.Date(as.character(spx_index_values$Date), "%m/%d/%Y"), spx_index_values$SPX.Index, type='l', 
     main='SPX index levels, from 1954 to 2015',
     xlab='year', ylab='SPX index')

# calculate the return series
spx_index_values$Return = rep(0, dim(spx_index_values)[1])
spx_index_values$Return[2:length(spx_index_values$Return)] = 
  spx_index_values$SPX.Index[2:length(spx_index_values$SPX.Index)] / 
  spx_index_values$SPX.Index[1:(length(spx_index_values$SPX.Index)-1)] - 1

# calculation empirical moments
return.data = spx_index_values$Return
n.length = length(return.data)
emp.moment.1 = mean(return.data)
emp.moment.2 = 1/(n.length-1)*sum((return.data - emp.moment.1)^2)
emp.moment.4 = 1/(n.length-1)*sum((return.data - emp.moment.1)^4)
emp.moment.6 = 1/(n.length-1)*sum((return.data - emp.moment.1)^6)

# optimization function
moment.diff = function(data.input){
  mu = data.input[1]
  sigma.square = data.input[2]
  lambda = data.input[3]
  delta.square = data.input[4]
  
  theo.moment.1 = mu - sigma.square / 2
  theo.moment.2 = sigma.square + lambda * delta.square
  theo.moment.4 = 3 * ((sigma.square+lambda*delta.square)^2 + lambda*delta.square^2)
  theo.moment.6 = 15 * ((sigma.square+lambda*delta.square)^2 + 3*lambda*delta.square*(sigma.square+lambda*delta.square)+lambda*delta.square^3)
  
  least.square.obj = (theo.moment.1 - emp.moment.1)^2 + (theo.moment.2 - emp.moment.2)^2 + (theo.moment.4 - emp.moment.4)^2 + (theo.moment.6 - emp.moment.6)^2
  return(least.square.obj)
}

# parameter calibration
output = optim(c(0, 0.006, 0.03, 0.0005), moment.diff)
output$par
