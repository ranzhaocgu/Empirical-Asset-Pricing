setwd('C:\\Users\\ranzhao\\Documents\\Empirical-Asset-Pricing\\Assignment 2')
setwd('D:\\PhD FE\\Empirical-Asset-Pricing\\Assignment 1')
require(pscl)

# Data loading
spx_index_values = read.csv('spx_index_values.csv', header = TRUE)
plot(as.Date(as.character(spx_index_values$Date), "%m/%d/%Y"), spx_index_values$SPX.Index, type='l', 
     main='SPX index levels, from 1954 to 2015',
     xlab='year', ylab='SPX index')

# calculate the return series
spx_index_values$Return = rep(0, dim(spx_index_values)[1])
spx_index_values$Return[2:length(spx_index_values$Return)] = 
  log(spx_index_values$SPX.Index[2:length(spx_index_values$SPX.Index)] / 
  spx_index_values$SPX.Index[1:(length(spx_index_values$SPX.Index)-1)])
data.length = length(spx_index_values$Return)


##################################################
# Black-Scholes model with Bayesian MCMC
simulation.length = 2000
mu.vector = rep(0, simulation.length)
sigma2.vector = rep(0, simulation.length)

# initialize the parameters (priors)
mu.vector[1] = mean(spx_index_values$Return)
sigma2.vector[1] = var(spx_index_values$Return)
alpha = 1000
beta = 0.2
theta = 0
delta2 = 0.001


for (i in 2:simulation.length){
  sigma2.vector[i] = rigamma(1,alpha+0.5*data.length, beta+0.5*sum((spx_index_values$Return - mu.vector[i-1])^2))
  delta.star.2 = 1/(data.length/sigma2.vector[i-1] + 1/delta2)
  mu.vector[i] = rnorm(1,(sum(spx_index_values$Return)/sigma2.vector[i-1]+theta/delta2)*delta.star.2, sqrt(delta.star.2))   
}

plot(mu.vector, type='l', xlab='mu',xlim=c(1001,2000),main='MCMC estimation for mu')
plot(sigma2.vector, type='l', xlab='sigma^2',xlim=c(1001,2000),main='MCMC estimation for sigma^2')
mu.vector.est = mu.vector[1001:2000]
sigma2.vector.est = sigma2.vector[1001:2000]

quantile.mu.vector = c(quantile(mu.vector.est, 0.025), quantile(mu.vector.est, 0.5), quantile(mu.vector.est, 0.975))
quantile.sigma2.vector = c(quantile(sigma2.vector.est, 0.025), quantile(sigma2.vector.est, 0.5), quantile(sigma2.vector.est, 0.975))

# check with existing library in r
model <- set.to.class("Diffusion", parameter = list(phi = mean(spx_index_values$Return), gamma2 = var(spx_index_values$Return)))
est_diff <- estimate(model, 1:length(spx_index_values$Return), spx_index_values$Return, 2000)
mu.vector.est.comp = est_diff@phi[1001:2000]
sigma2.vector.est.comp = est_diff@gamma2[1001:2000]

quantile.mu.vector = c(quantile(mu.vector.est.comp, 0.025), quantile(mu.vector.est.comp, 0.5), quantile(mu.vector.est.comp, 0.975))
quantile.sigma2.vector = c(quantile(sigma2.vector.est.comp, 0.025), quantile(sigma2.vector.est.comp, 0.5), quantile(sigma2.vector.est.comp, 0.975))

##################################################












##################################################
# Jump diffusion   model with Bayesian MCMC
# non-informative
model <- set.to.class("jumpDiffusion", Lambda = function(t, xi) (t/xi[2])^xi[1],
                      parameter = list(theta = 0.1, phi = 0.05, gamma2 = 0.1, xi = c(3, 1/4)))
est <- estimate(model, 1:length(spx_index_values$Return), spx_index_values$Return, 2000)
plot(est)

# informative
model <- set.to.class("jumpDiffusion", Lambda = function(t, xi) (t/xi[2])^xi[1],
                      parameter = list(theta = 0.1, phi = 0.05, gamma2 = 0.1, xi = c(3, 1/4)),
                      priorDensity = list(phi = function(phi) dnorm(phi, 0.05, 0.01),
                                          theta = function(theta) dgamma(1/theta, 10, 0.1*9),
                                          gamma2 = function(gamma2) dgamma(1/gamma2, 10, 0.1*9),
                                          xi = function(xi) dnorm(xi, c(3, 1/4), c(1,1))))
est <- estimate(model, 1:length(spx_index_values$Return), spx_index_values$Return, 2000)
plot(est)







