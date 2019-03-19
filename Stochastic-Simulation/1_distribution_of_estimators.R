##################################
## Variance of the trimmed mean ##
##################################

alpha <- seq(0.001,0.499,0.001)
a <- qnorm(1-alpha)
# variance derived using asymptotic result
sigma2 <- 2*(0.5+alpha*(a^2-1) - a*dnorm(a))/(1-2*alpha)^2
plot(alpha,sigma2,type="l")

# Do simulation
N <- 1000
x <- matrix(rnorm(24*N),nrow=N,ncol=24)
TM_sim <- matrix(0,N,12)
for (k in (0:11)) TM_sim[,k+1] <- apply(x,1,mean,trim=k/24)
var.trim <- apply(TM_sim^2,2,mean)
points((0:11)/24,24*var.trim,col=3)  # Standard estimator
##Comment: We plot 'n x VarEst(trimmed mean)', n=24, for better comparison with the ratio estimator below
points((0:11)/24,var.trim/var.trim[1],col=4) # Alternative: ratio estimator

# comparing with Hertzsprung's simulation results 
points((1:11)/24,c(1.013,1.037,1.069,1.095,1.139,1.184,1.232,1.283,
                   1.345,1.407,1.489),pch=2)

##Show distribution of trimmed mean
hist(TM_sim[,3])


###############
## Bootstrap ##
###############

# Data set used: Sublimation heat of platinum, from Rice, Chap. 10.4
plat <- c(136.3,136.6,135.8,135.4,134.7,135.0,134.1,143.3,147.8,148.8,134.8,
          135.2,134.9,146.5,141.2,135.4,134.8,135.8,135.0,133.7,134.4,134.9,
          134.8,134.5,134.3,135.2)
hist(plat)
n <- length(plat)

# direct implementation of the bootstrap
m <- floor((n-1)/2)
N <- 1000
x <- matrix(sample(plat,size=n*N,replace=TRUE),nrow=N,ncol=n)
BS_sim_TM <- matrix(0,N,m+1)
for (k in (0:m)) BS_sim_TM[,k+1] <- apply(x,1,mean,trim=k/n)
var.trim <- apply(BS_sim_TM,2,var)
plot((0:m)/n,var.trim/var.trim[1],ylab="variance ratio",
     xlab="trimming proportion")
hist(BS_sim_TM[,3])
round(sqrt(var.trim),3)


# Comparing the real data with bootstrap samples
#Using histograms
par(mfrow=c(2,2)) 
hist(plat,breaks <- seq(132,150,2)) 
plat.b <- plat[ceiling(n*runif(n))] # Bootstrap sample 
##plat.b <- sample(plat,size=n,replace=T) # does the same thing
hist(plat.b,breaks <- seq(132,150,2))
plat.f <- factor(plat) # values which occur, sorted, without ties
table(plat.f) # frequencies in the original sample
table(factor(plat.b,levels=levels(plat.f)))# frequencies in the bootstrap sample
#Using the normal plot
qqnorm(plat) 
plat.b <- plat[ceiling(n*runif(n))]
qqnorm(plat.b) #Normalplot of a bootstrap sample 
#Using the distribution function
par(mfrow=c(1,1))
plot.stepfun(plat,verticals=T,do.points=F) # empirical distribution function
plat.b <- plat[ceiling(n*runif(n))]
plot.stepfun(plat.b,verticals=T,do.points=F,add=T,col.hor=3,col.vert=3)
# empirical distribution function of the bootstrap sample


# Using functions of 'boot' package
library("boot")
help(boot)
t.mean <- function(d,ind) mean(d[ind],trim=0.2)
plat.boot <- boot(plat, t.mean, R=999, stype="i")
hist(plat.boot$t)
sqrt(var(plat.boot$t))
qqnorm(plat.boot$t)
boot.ci(plat.boot, conf=c(0.80,0.95),type=c("norm","basic","perc","bca"))
# confidence intervals using the bootstrap. All methods except norm
# correct for skewness of the trimmed mean.

