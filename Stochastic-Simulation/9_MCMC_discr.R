## Discrete version of (random walk) Metropolis-Hastings

##The space has dimension 99
## Define the target distribution (mixture of 2 discretized Beta-distributions)
a1 <- 0.3
n1 <- 500
a2 <- 0.5
n2 <- 500

k <- 0.01*(1:99)
p1 <- k^(a1*n1)*(1-k)^((1-a1)*n1)
p2 <- k^(a2*n2)*(1-k)^((1-a2)*n2)
## work with logs to avoid underflow in target probabilities
lp1 <- a1*n1*log(k) + (1-a1)*n1*log(1-k) +log(0.4) - log(sum(p1))
lp2 <- a2*n2*log(k) + (1-a2)*n2*log(1-k) +log(0.6) - log(sum(p2))
lmn <- pmin(lp1,lp2)
lmx <- pmax(lp1,lp2)
lp0 <- lmx + log(1+exp(lmn-lmx)) ##
p0=exp(lp0)## same as p0 <- 0.4*p1/sum(p1) + 0.6*p2/sum(p2) or as exp(lp1)+exp(lp2)# 
plot(k,p0,type="h")


##Defining the transition matrix (random walk)
q=construct_q(m=1,n=99)
mimage(q)
##Acceptance probability (note that Q(i,j)=Q(j,i))
a <- t(outer(lp0,lp0, FUN="-"))
##Acceptance probability
a[a>0]=0##corresponds to a=min(1,pi(j)/pi(i))
a=exp(a)
##Show conditional and unconditional acceptance probabilities
mimage(a)
mimage(a*q)


##Run MCMC simulation with above example
q=construct_q(m=1,n=99)
nstep=1000
x0=95

q=construct_q(m=15,n=99)
nstep=10000
x0=95

xs=f.mc(x0,nstep,q=q,a=a)

##Trace plot
plot(1:(nstep+1),xs/100,type="l")

md=rep(0,99)
for(i in 1:99) md[i]=sum(xs==i)
md=md/length(xs)
par(mfrow=c(2,2))
plot(k,md,type="h",main="Approx. target")
##Compare to target
plot(k,p0,type="h",main="target")
plot(k,p0-md,type="h")
##Autocorrelation
acf(xs)


##Calculation of transition kernel and stationary distribution
p.t <- a*q
rej <- 1-apply(p.t,1,sum) # rejection probabilities
sum(rej*p0) # rejection probability in stationary state
p.t <- p.t + diag(rej) # adjust diagonal
par(mfrow=c(1,1))
mimage(p.t)

sum(abs(p0-as.vector(t(p.t)%*%p0))) #check stationarity


p.p <- p.t
p.p <- p.p%*%p.p #P^2, P^4, P^8 etc.
mimage(p.p)

## Stationary distribution assuming uniform initial distribtion
for(i in 1:10) p.p <- p.p%*%p.p
statd=apply(p.p/99,2,sum)
plot(k,statd,type="h",main="stationary distribution")
##Compare to target
plot(k,p0,type="h",main="target")
plot(k,p0-statd,type="h")

f.mc <- function(x0,nstep,q,a){
  ## Purpose: discrete Metropolis-Hastings algorithm
  ## -------------------------------------------------------------------------
  ## Arguments: q=proposal probability, a=acceptance probability
  ##            n=number of states, x0=initial state, nstep=number of steps
  ## -------------------------------------------------------------------------
  x=rep(x0,nstep+1)
  n=dim(a)[1]
  for (i in (1:nstep)){
    ##sample proposal
    y=sample(x = 1:n, 1, replace = T, prob = q[x[i],]) 
    ##acceptance step
    if(runif(1)<=a[x[i],y]) x[i+1]=y else x[i+1]=x[i]
  }
  x
}

mimage <- function(mat, ncol = 40, zlim=range(mat)){
  ## Purpose: Color representation of a matrix 
  ## Interpolating a 'sequential' ColorBrewer palette
  YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
  
  stopifnot(length(d <- dim(mat)) == 2)
  image(x=1:d[2], y=1:d[1], z=t(mat)[,d[1]:1], axes = FALSE,
        xlab = "j", ylab  = "i",zlim=zlim,
        col = colorRampPalette(YlOrBr, space = "Lab")(ncol))
  axis(1)
  axis(2, at= 1:d[2], labels= as.character(d[2]:1))
}


construct_q = function(m,n){
  ## Purpose: Construct a proposal matrix for a random walk
  #m: range of the random walk
  q <- outer(1:n,1:n,FUN=function(x,y) {1<= abs(x-y) & abs(x-y) <= m})
  q <- q + outer(1:n,1:n,FUN=function(x,y) {abs(x-y) >=n-m})
  q <- q/(2*m) # proposal matrix
  return(q)
}

