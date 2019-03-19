# Simulating a (G,G,1) queue
n <- 100 # number of customers
iT <- rgamma(n,shape=1,scale=1) #interarrival times
S <- rgamma(n,shape=1,scale=0.7) #service times

T <- cumsum(iT) #arrival times
W <- rep(0,n) # workload at times T[i]-
for (i in (2:n)) W[i] <- max(0,W[i-1]+S[i-1]-iT[i])
D <- T+S+W #departure times
N <- c(rep(1,n),rep(-1,n)) #changes in the number of customers at T and D
E <- c(T,D) #event times (arrivals or departures)
ord <- order(E)
E <- E[ord] #ordered event times
N <- c(0,cumsum(N[ord])) # number of customers at event times

#Plots
#number of customers
f.N <- stepfun(E,N)
plot(f.N,do.points=FALSE,xlab="time",ylab="number of customers",
     main="Queueing system")
#workload
plot(T,W+S,xlab="time",ylab="workload",ylim=c(0,max(W+S)))
points(T,W)
abline(h=0)
segments(T,W,T,W+S)
segments(T,W+S,D,rep(0,n))

#Statistics
#number of customers
k <- (1:(2*n))[E==T[n]] # last arrival = k-th event
w <- E[1:k] - c(0,E[1:(k-1)]) # inter-event times until last arrival
N <- N[1:k]
M <- max(N) # maximal number of customers
p <- rep(0,M+1)
for (j in (0:M)) p[j+1] <- sum(w*(N==j))/T[n]
plot(0:M,p,type="h",main="Distribution of number of customers")


