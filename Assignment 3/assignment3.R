setwd('C:\\Users\\ranzhao\\Documents\\Empirical-Asset-Pricing\\Assignment 3')
setwd('D:\\PhD FE\\Empirical-Asset-Pricing\\Assignment 3')

ar.parameter.inference <- function(n=50,alpha=0,beta=1,sigma=0.2,p0=log(100),N=10000){
  # simulate the log stock price process
  alpha.bias = rep(0,N)
  beta.bias = rep(0,N)
  sigma.bias = rep(0,N)
  t.stats = rep(0,N)
  
  # simulation loop
  for (j in 1:N){
    p.series = rep(0, n)
    # time loop
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
    summ = summary(fitted.model)
    # fitted parameters
    alpha.fit = as.numeric(fitted.model$coefficients[1])
    beta.fit = as.numeric(fitted.model$coefficients[2])
    sigma.fit = sqrt(var(fitted.model$residuals)*(n-1)/(n-2))
    # bias parameters
    alpha.bias[j] = alpha.fit - alpha
    beta.bias[j] = beta.fit - beta
    sigma.bias[j] = sigma.fit - sigma
    t.stats[j] = as.numeric((summ$coefficients[2]-1)/summ$coefficients[4])
  }

  #output
  p.out = c()
  p.out$bias = cbind(alpha.bias, beta.bias, sigma.bias)
  p.out$t.stats = t.stats
  return(p.out)
}

# question (a) i, iii
T50alpha0beta1sigma0p2 = ar.parameter.inference(n=50,alpha=0,beta=1,sigma=0.2,p0=log(100),N=10000)
c(mean(T50alpha0beta1sigma0p2$bias[,1]), mean(T50alpha0beta1sigma0p2$bias[,2]), mean(T50alpha0beta1sigma0p2$bias[,3]))
quantile(T50alpha0beta1sigma0p2$t.stats,c(0.01,0.05))

# question (a) ii
alpha.bump.results1 = ar.parameter.inference(n=50,alpha=0.2,beta=1,sigma=0.2,p0=log(100),N=10000)
c(mean(alpha.bump.results1$bias[,1]), mean(alpha.bump.results1$bias[,2]), mean(alpha.bump.results1$bias[,3]))
quantile(alpha.bump.results1$t.stats,c(0.01,0.05))

alpha.bump.results2 = ar.parameter.inference(n=50,alpha=-0.2,beta=1,sigma=0.2,p0=log(100),N=10000)
c(mean(alpha.bump.results2$bias[,1]), mean(alpha.bump.results2$bias[,2]), mean(alpha.bump.results2$bias[,3]))
quantile(alpha.bump.results2$t.stats,c(0.01,0.05))

sigma.bump.results1 = ar.parameter.inference(n=50,alpha=0,beta=1,sigma=0.1,p0=log(100),N=10000)
c(mean(sigma.bump.results1$bias[,1]), mean(sigma.bump.results1$bias[,2]), mean(sigma.bump.results1$bias[,3]))
quantile(sigma.bump.results1$t.stats,c(0.01,0.05))

sigma.bump.results2 = ar.parameter.inference(n=50,alpha=0,beta=1,sigma=0.3,p0=log(100),N=10000)
c(mean(sigma.bump.results2$bias[,1]), mean(sigma.bump.results2$bias[,2]), mean(sigma.bump.results2$bias[,3]))
quantile(sigma.bump.results2$t.stats,c(0.01,0.05))

# question (a) iv
T600alpha0beta1sigma0p2 = ar.parameter.inference(n=600,alpha=0,beta=1,sigma=0.2,p0=log(100),N=10000)
c(mean(T600alpha0beta1sigma0p2$bias[,1]), mean(T600alpha0beta1sigma0p2$bias[,2]), mean(T600alpha0beta1sigma0p2$bias[,3]))
quantile(T600alpha0beta1sigma0p2$t.stats,c(0.01,0.05))

# question (b) i
T50alpha0beta0p95sigma0p2 = ar.parameter.inference(n=50,alpha=0,beta=0.95,sigma=0.2,p0=log(100),N=10000)
c(mean(T50alpha0beta0p95sigma0p2$bias[,1]), mean(T50alpha0beta0p95sigma0p2$bias[,2]), mean(T50alpha0beta0p95sigma0p2$bias[,3]))
quantile(T50alpha0beta0p95sigma0p2$t.stats,c(0.01,0.05))

