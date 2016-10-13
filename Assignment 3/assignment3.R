setwd('C:\\Users\\ranzhao\\Documents\\Empirical-Asset-Pricing\\Assignment 3')
setwd('D:\\PhD FE\\Empirical-Asset-Pricing\\Assignment 3')

ar.parameter.inference <- function(n=50,alpha=0,beta=1,sigma=0.2,p0=0){
  # simulate the return process
  p.series = rep(0, n)
  for (i in 1:n){
    if (i == 1){
      p.series[i] = alpha + beta * p0 + rnorm(1, 0, sigma)
    }
    else{
      p.series[i] = alpha + beta * p.series[i-1] + rnorm(1, 0, sigma)
    }
  }
  # fit the parameters using ols
  fitted.model = lm(p.series[2:n]~p.series[1:(n-1)])
  # fitted parameters
  alpha.fit = as.numeric(fitted.model$coefficients[1])
  beta.fit = as.numeric(fitted.model$coefficients[2])
  sigma.fit = sqrt(var(fitted.model$residuals)*(n-1)/(n-2))
  # bias parameters
  alpha.bias = alpha.fit - alpha
  beta.bias = beta.fit - beta
  sigma.bias = sigma.fit - sigma
  # beta inference
  se = sigma.fit / sqrt(sum((p.series-mean(p.series))^2))
  t.stat.1p = qt(0.01, n-2) - (1/se)
  t.stat.5p = qt(0.05, n-2) - (1/se)
  #output
  p.out = c()
  p.out$fitted.coefficients = matrix(c(alpha.fit, beta.fit, sigma.fit), nrow=1, dimnames=list(c(),c("alpha","beta","sigma")))
  p.out$bias = matrix(c(alpha.bias, beta.bias, sigma.bias), nrow=1, dimnames=list(c(),c("alpha","beta","sigma")))
  p.out$t.stats = matrix(c(t.stat.1p, t.stat.5p))
  return(p.out)
}

ar.parameter.inference(n=50,alpha=0,beta=1,sigma=0.2,p0=0)
ar.parameter.inference(n=600,alpha=0,beta=1,sigma=0.2,p0=0)
ar.parameter.inference(n=50,alpha=0,beta=0.95,sigma=0.2,p0=0)
