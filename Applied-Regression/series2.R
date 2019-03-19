# q1
jams <- read.table("Gotthard-Tunnel.csv", sep = ",", header = TRUE)
jams_smoother <- read.table("Gotthard-Tunnel-Smoother.csv", sep = ",", header = TRUE)
plot(jams)
lines(jams_smoother, col = "red")
smth <- ksmooth(jams$year, jams$jams, kernel = "box", range.x = range(jams$year), bandwidth = 3)
plot(jams)
lines(smth, col = "blue")
lines(jams_smoother, col = "red")

bw <- 4*abs(qnorm(0.25,0,2)) # bandwidth chosen st sigma of n is 2
fit <- ksmooth(jams$year, jams$jams, kernel="normal", bandwidth=bw, x.points=2006:2016)
plot(jams$year, jams$jams, xlab="Year", ylab="Days", pch=20, ylim=c(70,170))
title("Days with traffic jams - Gaussian kernel smoother (with R)", cex.main = 0.75)
lines(fit$x, fit$y, col="blue")

lines(loess.smooth(jams$year, jams$jams, degree=1, span=0.75), col="red")
# Loess smoother
lines(loess.smooth(jams$year, jams$jams, degree=2, span=0.75), col="blue")

# q2
load("solar.radiation.rda")
sol.rad$rad[sol.rad$rad == 99999] <- NA
sol.rad <- na.omit(sol.rad)

plot(sol.rad)

box.smoother <- ksmooth(sol.rad$jahr, sol.rad$rad, kernel = "box", bandwidth = 10)
gaussian.smoother <- ksmooth(sol.rad$jahr, sol.rad$rad, kernel = "normal", bandwidth = 10)
loess.smoother <- loess.smooth(sol.rad$jahr, sol.rad$rad, degree = 1, family = "gaussian")

lines(box.smoother, col = "red")
lines(gaussian.smoother, col = "blue")
lines(loess.smoother, col = "green")
