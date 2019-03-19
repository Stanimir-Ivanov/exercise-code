###################################
# Linear congruential generators ##
###################################

# setting the parameters
m <- 256 # small example 
a <- 137
const <- 1
d <- 2

m <- 2^31 # generator Randu
a <- 2^16+3
const <- 0
d <- 3

m <- 2^29 # generator which was used in an Unix version
a <- 62605
const <- 113218009
d <- 3

# generating nrep elements of the sequence
nrep <- min(m,2^16)
x <- rep(1,nrep+d-1) 
for (i in (2:(nrep+d-1)))  x[i] <- (a*x[i-1]+const)%%m
##plotting
hist(x/m,breaks=20)
n1 <- min(m,5000)
plot(x[1:n1],x[2:(n1+1)],cex=0.5)

#Visualisation for d=3
library(rgl)
n1 <- min(m,5000)
x3 <- cbind(x[1:n1],x[2:(n1+1)],x[3:(n1+2)])
plot3d(x3,type='s',size=1)


########################
## Testing generators ##
########################
u=x/m
u=runif(10000)

##Kolmogorov-Smirnov test
ks.test(u, "punif")
##Chi-squared test
nb=500##number of bins
chisq.test(table(floor(nb*u)))

                   
#Example of a "monkey test" (overlapping pairs of 10 bits). 
#Idea: Partition [0,1]^2 into a regular grid with lengths 1/1024 and 
#      count how many cells are empty when simulation 2^21+1 values
# - Generate 2^21+1 (=512*4096+1) values, in groups of length 4096
# - For each such group, count how many of the 1024^2=2^20 pairs do not occur
# - r is the last random number in a such group which has to be carried over to the next group
empty <- rep(TRUE,1024^2)
r <- floor(1024*runif(1))
for (i in (1:512)){
  x <- c(r,floor(1024*runif(4096)))
  r <- x[4097]
  y <- 1+x[-4097]+1024*x[-1]
  empty[y] <- FALSE
}
(sum(empty)-141909)/290.26 #Standardisation following Marsaglia


# Information on random number generators in R
help(.Random.seed)


