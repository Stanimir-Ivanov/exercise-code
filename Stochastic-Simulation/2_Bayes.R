################################
## Normal-Gamma prior example ##
################################

##Assume the following model:
# Observations: normal(theta,sigma^2)
# Prior for theta: normal(xi,ka2), 
# prior for 1/sigma^2 (called precision): Gamma(gam,lam), independent

x <- rnorm(4) #generating data
xb <- mean(x)
n <- length(x)
xs <- var(x)*(n-1)/n  

f.lik.contour <- function(n,xb,xs){
  ## Purpose: Plotting the log likelihood of normal observations
  ## ----------------------------------------------------------------------
  ## Arguments: xb,xs=mle for mean and variance, n=sample size
  ## ----------------------------------------------------------------------
  ## Author: Hans-Ruedi Kuensch
  th <- seq(xb-3*sqrt(xs/n),xb+3*sqrt(xs/n),length=32)
  # range for theta
  si <- sqrt(n*xs)*seq(1/sqrt(qchisq(0.99,n-1)),1/sqrt(qchisq(0.01,n-1)),
                       length=32)# range for sigma
  z <- n*outer(rep(1,32),log(si))+ 0.5*n*outer(xs+(th-xb)^2,si^2,FUN="/")
  #computing log likelihood
  contour(th,si,-z,nlevels=20,xlab="theta",ylab="sigma")
}

f.post.contour <- function(xi,ka2,gam,lam,n,xb,xs){
  ## Purpose: Plotting contours of the posterior. The main task is to define
  ##          a suitable domain where the posterior is concentrated 
  ## -------------------------------------------------------------------------
  ## Arguments: xi,ka2,al,lam=hyperparameters, xb,xs=mle
  ##            n=sample size (n=0 gives the prior !)
  ## -------------------------------------------------------------------------
  ## Author: Hans-Ruedi Kuensch
  s2 <- lam/gam #gam/lam is the prior mean of 1/sigma^2
  a <- s2/(s2+n*ka2)
  mu <- a*xi+(1-a)*xb # posterior mean of theta if sigma^2=s2
  th <- seq(mu-3*sqrt(a*ka2),mu+3*sqrt(a*ka2),length=32)
                                        # defines the range for theta
  lam <- lam+0.5*n*xs 
  gam <- gam+0.5*n
  #if theta=xb, posterior for 1/sigma^2 is Gamma(lam,gam)
  si <- sqrt(lam)*seq(1/sqrt(qgamma(0.99,gam)),1/sqrt(qgamma(0.01,gam)),
                      length=32) # defines the range for sigma 
  z <- 0.5*outer((th-xi)^2,rep(1,32))/ka2+(2*gam+1)*outer(rep(1,32),log(si))+
    outer(lam+0.5*n*(th-xb)^2,si^2,FUN="/")
           #log posterior on the lattice of values for theta and sigma 
  contour(th,si,-z,nlevels=20,xlab="theta",ylab="sigma")
}


f.gibbs <- function(xi,ka2,gam,lam,n,xb,xs,nrep=1000){
  ## Purpose: Implementing the Gibbs-sampler for the posterior in a normal
  ## model with normal-inverse Gamma prior
  ## -------------------------------------------------------------------------
  ## Arguments: xi,ka2,gam,lam=hyperparameters, xb,xs=sufficient statistic
  ## -------------------------------------------------------------------------
  ## Author: Hans-Ruedi Kuensch
  th <- rep(xb,nrep)
  sisq <- rep(xs,nrep)
  for (k in (2:nrep)){
    a <- sisq[k-1]/(sisq[k-1]+n*ka2)
    th[k] <- rnorm(1,a*xi+(1-a)*xb,sqrt(a*ka2)) # given sisq, sample theta
    sisq[k] <- (lam+0.5*n*(xs+(xb-th[k])^2))/rgamma(1,0.5*n+gam) # given theta, sample sisq
  }
  list(th=th,si=sqrt(sisq))
}




#Choice of hyperparameter
#1. not informative
xi <- 0
ka2 <- 100
gam <- 1
lam <- 1
#2. informative for theta, in agreement with data
xi <- xb
ka2 <- xs/n
gam <- 1
lam <- 1
#3. informative for theta, conflicting the data
xi <- xb+3*sqrt(xs/n)
ka2 <- xs/n
gam <- 1
lam <- 1 

#Beginning of the Gibbs-Sampler
gibbs <- f.gibbs(xi,ka2,gam,lam,n,xb,xs,nrep=50)
f.post.contour(xi,ka2,gam,lam,n,xb,xs)
points(gibbs$th[1],gibbs$si[1],col=4,pch=16)
i <- 0
i <- i+1
points(gibbs$th[i+1],gibbs$si[i],col=4,pch=16)
arrows(gibbs$th[i],gibbs$si[i],gibbs$th[i+1],gibbs$si[i],length=0.1,col=4)
points(gibbs$th[i+1],gibbs$si[i+1],col=4,pch=16)
arrows(gibbs$th[i+1],gibbs$si[i],gibbs$th[i+1],gibbs$si[i+1],length=0.1,col=4) 

#prior density, likelihood und posterior density 
par (mfrow=c(2,2))
f.post.contour(xi,ka2,gam,lam,0,xb,xs)
title(main="log prior density")
f.lik.contour(n,xb,xs)
title(main="log likelihood")
points(xb,sqrt(xs),pch=15)
f.post.contour(xi,ka2,gam,lam,n,xb,xs)
points(xb,sqrt(xs),pch=15)
title(main="log posterior density")
#adding output from the Gibbs sampler
gibbs <- f.gibbs(xi,ka2,gam,lam,n,xb,xs,nrep=1000)
f.post.contour(xi,ka2,gam,lam,n,xb,xs)
title(main="Gibbs-Sampler and log posterior density")
points(gibbs$th,gibbs$si)

#marginal posterior for theta
hist(gibbs$th,breaks=20, freq=F,xlab="theta", main=
     "Histogram of values of theta and estimated posterior")
d <- density(gibbs$th)
lines(d$x,d$y)
from <- min(min(gibbs$th),xi-2.5*sqrt(ka2))
to <- max(max(gibbs$th),xi+2.5*sqrt(ka2))
x <- seq(from,to,length=500)
plot(d$x,d$y,type="l",xlim=c(from,to),xlab="theta",ylab="density",
             main="prior and posterior marginal")
lines(x,dnorm(x,xi,sqrt(ka2)),col=2)
abline(h=0)
#marginal posterior for sigma
hist(gibbs$si,breaks=20,freq=F,xlab="sigma", ylab="density",main=
     "Histogram of values of sigma and estimated posterior")
d <- density(gibbs$si)
lines(d$x,d$y)
from <- min(min(gibbs$si),1/sqrt(qgamma(0.99,gam)))
to <- max(max(gibbs$si),1/sqrt(qgamma(0.01,gam)))
x <- seq(from,to,length=500)
y <- 2*dgamma(1/(x^2),shape=gam,rate=lam)/(x^3)
plot(d$x,d$y,xlim=c(from,to),type="l",xlab="sigma",ylab="density",
             main="prior and posterior marginal")
lines(x,y,col=2)
abline(h=0)


#Bayesian prediction: Effect of incorporating uncertainty about parameters
limit <- xb+qnorm(0.95)*sqrt(xs) 
# P(new observation <=limit)=0.95, if true parameter = MLE 
pnorm(limit,mean=xb,sd=sqrt(xs))
# P(new observation <=limit | data)
mean(pnorm(limit,mean=gibbs$th,sd=gibbs$si))
 





#Simulation from the posterior using Metropolis, will be shown later in
#the course.

# Generic function for Metropolis with random walk proposal
f.metrop <- function(x0,scale,nrep,r.target, ...){
  ## Purpose: Metropolis algorithm with Gaussian random walk proposals
  ## -------------------------------------------------------------------------
  ## Arguments: x0 = (vector of) starting values, scale = vector of
  ##            standard deviations for the random walk, nrep=number of
  ##            replicates, r.target = ratio of target densities
  ## -------------------------------------------------------------------------
  ## Author: Hans-Ruedi Kuensch
  d <- length(x0)
  x <- matrix(rnorm(d*nrep),nrow=nrep,ncol=d)
  y <- matrix(0,nrow=nrep-1,ncol=d)
  x <- x%*%diag(scale,nrow=d)
  x[1,] <- x0
  nrej <- 0
  for (i in (2:nrep)) {
    x[i,] <- x[i-1,]+x[i,]
    y[i-1,] <- x[i,]
    if (runif(1) > r.target(x[i,],x[i-1,],...)) {
      x[i,] <- x[i-1,]
      nrej <- nrej+1
    }
  }
  list(sample=x,acc=1-nrej/nrep,prop=y)
}

#Ratios of the posterior density
r.post <- function(x1,x2,xi,ka2,gam,lam,n,xb,xs){
  ## Purpose: Computing ratio of posteriori densities at x1 and x2
  ##          for an i.i.d. normal model
  ## ----------------------------------------------------------------------
  ## Arguments: x=(mu,log(sigma)) = Parameters of the normal distribution
  ##            xi,ka2,gam,lam = Parameters of the prior
  ##            n=number of data, xb=sample mean, xs=sample variance
  ## ----------------------------------------------------------------------
  ## Author: Hans-Ruedi Kuensch
  ratio <- 0.5*((x2[1]-xi)^2-(x1[1]-xi)^2)/ka2 + (2*gam+n+1)*(x2[2]-x1[2]) +
    (lam+0.5*n*(xs+(x2[1]-xb)^2))*exp(-2*x2[2]) -
    (lam+0.5*n*(xs+(x1[1]-xb)^2))*exp(-2*x1[2])
  exp(ratio)
}


f.post.contour(xi,ka2,gam,lam,n,xb,xs)
points(xb,sqrt(xs),pch=15)
title(main="log a posteriori Dichte")
# Few iterations (with log scale for sigma) to explain the idea
metrop <- f.metrop(x0=c(xb,0.5*log(xs)),scale=c(0.6,0.3),nrep=21,
                   r.target=r.post,xi,ka2,gam,lam,n,xb,xs)
sample <- metrop$sample
sample[,2] <- exp(sample[,2])
prop <- metrop$prop
prop[,2] <- exp(prop[,2])
points(sample[,1],sample[,2],col=2)
arrows(sample[-21,1],sample[-21,2],prop[,1],prop[,2],col=4)
#Result of many iterations 
metrop <- f.metrop(x0=c(xb,0.5*log(xs)),scale=c(0.6,0.3),nrep=1000,
                   r.target=r.post,xi,ka2,gam,lam,n,xb,xs)
metrop$acc  # acceptance rate
param <- metrop$sample
points(param[,1]+0.04*(runif(1000)-0.5),
       exp(param[,2])+0.04*(runif(1000)-0.5),cex=0.5)
# Making overlapping points visible
par(mfrow=c(2,1))
plot(param[1:500,1],type="l")
plot(param[1:500,2],type="l")
#Autocorrelations 
acf(param[,1])
acf(param[,2])
