setwd('C:\\Users\\ranzhao\\Documents\\Empirical-Asset-Pricing\\Assignment 8')
setwd('D:\\PhD FE\\Empirical-Asset-Pricing\\Assignment 8')
setwd('D:\\Empirical-Asset-Pricing\\Assignment 8')

require("data.table")

ff.factors = fread('F-F_Research_Data_Factors.CSV', data.table=FALSE)
colnames(ff.factors)[1] = 'date'

stock.data = fread('individual_stocks.csv', header=TRUE, data.table=FALSE, select=c('PERMNO','date','RET'))
stock.data = stock.data[stock.data[,3] != '' & stock.data[,3] != 'C' & stock.data[,3] != 'B',]
stock.data[,3] = as.numeric(stock.data[,3])
stock.data[,2] = substr(stock.data[,2], 1, 6)
ff.start = min(ff.factors$date)
ff.end = max(ff.factors$date)
stock.data = stock.data[stock.data$date >= ff.start & stock.data$date <= ff.end, ]

asset.set = unique(stock.data$PERMNO)

# exclude stock with less than 10 ovservations in research period
exclude.stock = c()
for (i in 1:length(asset.set)){
  permno = asset.set[i]
  stock = stock.data[stock.data[,1]==permno, c(2,3)]
  if (nrow(stock) < 10){
    exclude.stock = c(exclude.stock, permno)
    stock.data = stock.data[stock.data$PERMNO != permno, ]
  }
  
  if (i %% 1000 == 0){
    cat("iteration = ", i, "\n")
  }
}

length(exclude.stock)

# cleaned data
asset.set = unique(stock.data$PERMNO)
first.pass.betas = matrix(0, nrow=length(asset.set), ncol=4, 
                          dimnames=list(sort(asset.set), c('constant', 'rm_rf' ,'HML', 'SMB')))
first.pass.betas = as.data.frame(first.pass.betas)

# first pass regression: time series on each asset
for (i in 1:length(asset.set)){
  permno = asset.set[i]
  stock = stock.data[stock.data[,1]==permno, c(2,3)]
  merged.data = merge(stock, ff.factors, by.x = "date", by.y = "date")
  if (nrow(merged.data) < 10){  # exclude data with less than 10 time series observations
    exclude.stock = c(exclude.stock, permno)
  }
  else{
    reg.model = lm(RET-RF~`Mkt-RF`+SMB+HML,data=merged.data)
    first.pass.betas[row.names(first.pass.betas) == permno,] = as.numeric(reg.model$coefficients)
  }
  
  if (i %% 1000 == 0){
    cat("iteration = ", i, "\n")
  }
}

# second pass regression: cross sectional over time
research.period = unique(ff.factors$date)
second.pass.lambdas = matrix(0, nrow=length(research.period), ncol=4, 
                             dimnames=list(sort(research.period), c('constant', 'rm_rf' ,'HML', 'SMB')))
second.pass.lambdas = as.data.frame(second.pass.lambdas)
second.pass.errors = matrix(0, nrow=length(asset.set), ncol=length(research.period), 
                            dimnames=list(sort(asset.set), sort(research.period)))
first.pass.betas$perm = row.names(first.pass.betas)

for (j in 1:length(research.period)){
  date = research.period[j]
  stock = stock.data[stock.data$date == date, ]
  merged.data = merge(stock, first.pass.betas, by.x = "PERMNO", by.y = "perm")
  rf = ff.factors$RF[ff.factors$date == date]
  reg.model = lm(RET-rf~rm_rf+SMB+HML,data=merged.data)
  # lambdas
  second.pass.lambdas[row.names(second.pass.lambdas) == date, ] = as.numeric(reg.model$coefficients)
  # mean errors
  second.pass.errors[row.names(second.pass.errors) %in% merged.data$PERMNO,colnames(second.pass.errors) == date] = as.numeric(reg.model$residuals)
}

T = nrow(second.pass.lambdas)
lambda.est = colMeans(second.pass.lambdas)
lambda.sd = sqrt(1/T^2 * colMeans((second.pass.lambdas - colMeans(second.pass.lambdas))^2))

mean.errors = matrix(0, nrow=length(asset.set), ncol=1)
for (i in 1:length(asset.set)){
  permno = asset.set[i]
  mean.errors.i = second.pass.errors[row.names(second.pass.errors) == permno, 
                                     second.pass.errors[row.names(second.pass.errors) == permno, ] != 0]
  mean.errors[i] = mean(as.numeric(mean.errors.i))
  
  if (i %% 1000 == 0){
    cat("iteration = ", i, "\n")
  }
}

second.pass.errors = as.matrix(second.pass.errors)
chi.mean.error = 0
for (i in 1:length(asset.set)){
#foreach(i=1:length(asset.set)) %dopar% {
  permno = asset.set[i]
#  mean.errors.i = second.pass.errors[row.names(second.pass.errors) == permno, 
#                                     second.pass.errors[row.names(second.pass.errors) == permno, ] != 0]
#  dates.i = colnames(mean.errors.i)
  data.i=second.pass.errors[i,]
  i.start = min(which(data.i!=0))
  i.end = max(which(data.i!=0))
  for (j in i:length(asset.set)){
#  foreach(j=1:length(asset.set)) %dopar% {
    if (i == j){
      chi.mean.error = chi.mean.error + mean.errors[i]^2*as.numeric(1/length(data.i[i.start:i.end])^2 * sum((data.i[i.start:i.end]-mean(data.i[i.start:i.end]))^2))
    }
    else {
      permno.j = asset.set[j]
#      mean.errors.j = second.pass.errors[row.names(second.pass.errors) == permno.j, 
#                                         second.pass.errors[row.names(second.pass.errors) == permno.j, ] != 0]
      data.j = second.pass.errors[j, ] 
      j.start = min(which(data.j!=0))
      j.end = max(which(data.j!=0))
      start.date = max(i.start, j.start)
      end.date = min(i.end, j.end)
      #dates.j = colnames(mean.errors.j)
      #inter.dates = intersect(dates.i, dates.j)
      if (end.date > start.date){
        data.i.use = data.i[start.date:end.date]
        data.j.use = data.j[start.date:end.date]
        chi.mean.error = chi.mean.error + mean.errors[i]*mean.errors[j]*as.numeric(2*1/(end.date-start.date)^2 * sum((data.i.use-mean(data.i.use)) * (data.j.use-mean(data.j.use))))
      }
    }
  }
  
  if (i %% 100 == 0){
    cat("Main (i) iteration = ", i, "\n")
  }
}

