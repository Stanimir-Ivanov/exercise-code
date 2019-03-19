#################################
## Adaptive rejection sampling ##
#################################

# As input a function to compute log f and its derivative and two values on
# either side of the maximum are required  

# Arguments: cx = Interval boundaries for piecewise linear majorants
# a,b = intercepts and bound for piecewise linear majorants
# k = number of linear pieces in the upper bound
# M = proportional to probabilities of the different components of the proposal
# x = vector of accepted values
# The code allows the first and/or the last interval to be unbounded.
# It avoids over- and underflow in cases where b is almost zero. 

#setup (here for the Beta distribution)
#log density (up to a constant) and its derivative
f.log <- function(x) al*log(x) + be*log(1-x)
f.logd <- function(x) al/x - be/(1-x)
al <- 20-1 
be <- 20-1 # Parameters reduced by one compared with usual 
k <- 2
cx <- c(0.3,0.7) # 2 points on both sides of the maximum.
b <- f.logd(cx) # slopes of the tangents 
a <- f.log(cx) -cx*b # intercepts of the tangents. 
cx <- (a[2]-a[1])/(b[1]-b[2]) # intersection of the tangents 
cx <- c(0,cx,1)
r <- 1 - exp(-abs(b)*(cx[-1]-cx[-3])) # auxiliary variable 
M <- c(b[1]*cx[2],b[2]*cx[2]) + a + log(r) - log(abs(b))
M <- exp(M - max(M))
x <- NULL
xx <- seq(0.2,0.8,by=0.005) #Plotting
plot(xx,f.log(xx),type="l")
segments(cx[-(k+1)],a+b*cx[-(k+1)],cx[-1],a+b*cx[-1],col=2)

#sampling (i is the counter for the proposed values)
for (i in (1:1000)){ 
  ind <- sample(1:k,1,prob=M) # Select an interval
  xnew <- log(1- runif(1)*r[ind])/b[ind] # generate the proposal
  if (b[ind]>0) xnew <- cx[ind+1] +xnew else xnew <- cx[ind]+xnew
  if (log(runif(1)) < f.log(xnew) - a[ind] - b[ind]*xnew) x <- c(x,xnew)
                                        # Decide whether to accept
#Adapt the proposal if too few values are accepted
  if (length(x)<0.95*i) { 
    print(i)
    bnew <- f.logd(xnew)
    anew <- f.log(xnew)-bnew*xnew # tangent at xnew
    j <- 1+sum(b>bnew) # Number of the new interval 
    if (j==1) { # update a,b,cx if new interval is the first one 
      cx <- c(cx[1],(anew-a[1])/(b[1]-bnew),cx[-1])
      a <- c(anew,a)
      b <- c(bnew,b)
    } else if (j==(k+1)) { # same if new interval is the last one 
      cx <- c(cx[1:k],(anew-a[k])/(b[k]-bnew),cx[k+1])
      a <- c(a,anew)
      b <- c(b,bnew)
    } else { # same if new intervall is somewhere in the middle 
      cx <- c(cx[1:(j-1)],(anew-a[j-1])/(b[j-1]-bnew),
               (anew-a[j])/(b[j]-bnew),cx[-(1:j)])
      a <- c(a[1:(j-1)],anew,a[j:k])
      b <- c(b[1:(j-1)],bnew,b[j:k])
    }
    k <- k+1
    r <- 1 - exp(-abs(b)*(cx[-1]-cx[-(k+1)]))# Update r und M
    ind <- min((1:k)[b<0])            
    M <- b*c(cx[2:ind],cx[ind:k]) + a + log(r) - log(abs(b))
    M <- exp(M - max(M))
  }
}

#adapted proposal
segments(cx[-(k+1)],a+b*cx[-(k+1)],cx[-1],a+b*cx[-1],col=4)

#Results
length(x) # how many values were accepted ?
k # how many segments does the final proposal contain ?
hist(x,freq=FALSE,breaks=20)
lines(xx,dbeta(xx,20,20))  # checking if accepted values are beta-distributed





