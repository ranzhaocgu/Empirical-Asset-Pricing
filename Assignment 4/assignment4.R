setwd('C:\\Users\\ranzhao\\Documents\\Empirical-Asset-Pricing\\Assignment 2')
setwd('D:\\PhD FE\\Empirical-Asset-Pricing\\Assignment 2')
setwd('D:\\Empirical-Asset-Pricing\\Assignment 2')
require(pscl)

# Data loading
spx_index_values = read.csv('spx_index_values.csv', header = TRUE)
par(mfrow=c(2,1))
plot(as.Date(as.character(spx_index_values$Date), "%m/%d/%Y"), spx_index_values$SPX.Index, type='l', 
     main='SPX index levels, from 1954 to 2015',
     xlab='year', ylab='SPX index')

# calculate the return series
spx_index_values$Return = rep(0, dim(spx_index_values)[1])
spx_index_values$Return[2:length(spx_index_values$Return)] = 
  log(spx_index_values$SPX.Index[2:length(spx_index_values$SPX.Index)] / 
  spx_index_values$SPX.Index[1:(length(spx_index_values$SPX.Index)-1)])
data.length = length(spx_index_values$Return)

plot(as.Date(as.character(spx_index_values$Date), "%m/%d/%Y"), spx_index_values$Return, type='l', 
     main='SPX index returns, from 1954 to 2015',
     xlab='year', ylab='SPX returns')

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
  # draw mu first, does the order matter?
  delta.star.2 = 1/(data.length/sigma2.vector[i-1] + 1/delta2)
  mu.vector[i] = rnorm(1,(sum(spx_index_values$Return)/sigma2.vector[i-1]+theta/delta2)*delta.star.2, sqrt(delta.star.2))   
  sigma2.vector[i] = rigamma(1,alpha+0.5*data.length, beta+0.5*sum((spx_index_values$Return - mu.vector[i])^2))
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
# Jump diffusion model with Bayesian MCMC
simulation.length = 2000
merton.mu.vector = rep(0, simulation.length)
merton.sigma2.vector = rep(0, simulation.length)
merton.mu.s.vector = rep(0, simulation.length)
merton.sigma2.s.vector = rep(0, simulation.length)
merton.lambda.vector = rep(0, simulation.length)
merton.Z = rep(0, simulation.length)
merton.xi = rep(0, simulation.length)
jump.times = rep(0, simulation.length)

# initialize the parameters (priors)
merton.mu.vector[1] = mean(spx_index_values$Return)
merton.sigma2.vector[1] = var(spx_index_values$Return)
merton.mu.s.vector[1] = 0
merton.sigma2.s.vector[1] = 0.03
merton.lambda.vector[1] = 0.03
merton.Z.data = as.numeric(runif(data.length, 0, 1) < merton.lambda.vector[1])
merton.xi.data = rnorm(data.length, merton.mu.s.vector[1], sqrt(merton.sigma2.s.vector[1]))
merton.Z[1] = 0
merton.xi = 0
alpha = 1000
beta = 0.2
theta = 0
delta2 = 0.001
alpha.s = 1000
beta.s = 0.2
theta.s = 0
delta2.s = 0.001
gamma = 50
eta = 2

for (i in 2:simulation.length){
  # mu and sigma for the merton model 
  delta.star.2 = 1/(data.length/merton.sigma2.vector[i-1] + 1/delta2)
  merton.mu.vector[i] = rnorm(1,(sum(spx_index_values$Return-merton.Z.data*merton.xi.data)/merton.sigma2.vector[i-1]+theta/delta2)*delta.star.2, sqrt(delta.star.2))   
  merton.sigma2.vector[i] = rigamma(1,alpha+0.5*data.length, beta+0.5*sum((spx_index_values$Return - merton.mu.vector[i] - merton.Z.data*merton.xi.data)^2))
  # mu and sigma for the jump size
  delta.star.s.2 = 1/(data.length/merton.sigma2.s.vector[i-1] + 1/delta2.s)
  merton.mu.s.vector[i] = rnorm(1,(sum(merton.xi.data*merton.Z.data)/merton.sigma2.vector[i-1]+theta.s/delta2.s)*delta.star.s.2, sqrt(delta.star.s.2))
  merton.sigma2.s.vector[i] = rigamma(1,alpha.s+0.5*data.length, beta.s+0.5*sum((merton.Z.data*merton.xi.data - merton.mu.s.vector[i])^2))
  # jump intensity
  merton.lambda.vector[i] = rbeta(1, sum(merton.Z.data)+gamma, data.length - sum(merton.Z.data) + eta))
  # state variable xi
  sigma.star.xi = 1/(merton.Z[i-1]/merton.sigma2.vector[i]+1/merton.sigma2.s.vector[i])
  for (j in 1:data.length){
    merton.xi.data[j] = rnorm(1, (spx_index_values$Return[j]-merton.mu.vector[i])*merton.Z.data[j]/merton.sigma2.vector[i]+merton.mu.s.vector[i]/merton.sigma2.s.vector[i], sqrt(sigma.star.xi))
    jump.ind = runif(1, 0, 1)
    if (jump.ind < exp(-0.5*(spx_index_values$Return[j]-merton.mu.vector[i]-merton.xi.data[j])^2/merton.sigma2.vector[i])){
      merton.Z.data[j] = 1
    }
    else{
      merton.Z.data[j] = 0
    }
  }
  jump.times[i] = sum(merton.Z.data)
}



par(mfrow=c(2,1))
plot(merton.mu.vector, type='l', xlab='iterations',ylab='mu',xlim=c(1001,2000),main='MCMC estimation Merton model - mu')
plot(merton.sigma2.vector, type='l', xlab='iterations',ylab='sigma^2',xlim=c(1001,2000), ylim=c(0.00008,0.00012),main='MCMC estimation Merton model - sigma^2')
par(mfrow=c(3,1))
plot(merton.mu.s.vector, type='l', xlab='iterations',ylab='mu.s',xlim=c(1001,2000), ylim=c(-0.003,0.003),main='MCMC estimation Merton model - mu.s')
plot(merton.sigma2.s.vector, type='l', xlab='iterations',ylab='sigma.s^2',xlim=c(1001,2000), ylim=c(0,0.0002),main='MCMC estimation Merton model - sigma.s^2')
plot(merton.lambda.vector, type='l', xlab='iterations',ylab='lambda',xlim=c(1001,2000),main='MCMC estimation Merton model - lambda')

merton.mu.vector.est = merton.mu.vector[1001:2000]
merton.sigma2.vector.est = merton.sigma2.vector[1001:2000]
merton.mu.s.vector.est = merton.mu.s.vector[1001:2000]
merton.sigma2.s.vector.est = merton.sigma2.s.vector[1001:2000]
merton.lambda.vector.est = merton.lambda.vector[1001:2000]

quantile.mu.vector = c(quantile(merton.mu.vector.est, 0.025), quantile(merton.mu.vector.est, 0.5), quantile(merton.mu.vector.est, 0.975))
quantile.sigma2.vector = c(quantile(merton.sigma2.vector.est, 0.025), quantile(merton.sigma2.vector.est, 0.5), quantile(merton.sigma2.vector.est, 0.975))
quantile.mu.s.vector = c(quantile(merton.mu.s.vector.est, 0.025), quantile(merton.mu.s.vector.est, 0.5), quantile(merton.mu.s.vector.est, 0.975))
quantile.sigma2.vector = c(quantile(merton.sigma2.s.vector.est, 0.025), quantile(merton.sigma2.s.vector.est, 0.5), quantile(merton.sigma2.s.vector.est, 0.975))
quantile.lambda.vector = c(quantile(merton.lambda.vector.est, 0.025), quantile(merton.lambda.vector.est, 0.5), quantile(merton.lambda.vector.est, 0.975))


# check with existing library in R
# non-informative
model <- set.to.class("jumpDiffusion", Lambda = function(t, xi) (t/xi[2])^xi[1],
                      parameter = list(theta = 0.1, phi = 0.05, gamma2 = 0.1, xi = c(3, 1/4)))
est <- estimate(model, 1:length(spx_index_values$Return), spx_index_values$Return, 2000)
plot(est)

# informative
model2 <- set.to.class("jumpDiffusion", Lambda = function(t, xi) (t/xi[2])^xi[1],
                      parameter = list(theta = 0.1, phi = 0.05, gamma2 = 0.1, xi = c(3, 1/4)),
                      priorDensity = list(phi = function(phi) dnorm(phi, 0.05, 0.01),
                                          theta = function(theta) dgamma(1/theta, 10, 0.1*9),
                                          gamma2 = function(gamma2) dgamma(1/gamma2, 10, 0.1*9),
                                          xi = function(xi) dnorm(xi, c(3, 1/4), c(1,1))))
est2 <- estimate(model2, 1:length(spx_index_values$Return), spx_index_values$Return, 2000)
plot(est)
##################################################