set.seed(22)
n <- 100
xx <- 1:n
yy.a <- 2+1*xx+rnorm(n)
yy.b <- 2+1*xx+rnorm(n)*(xx)
yy.c <- 2+1*xx+rnorm(n)*(1+xx/n)
yy.d <- cos(xx*pi/(n/2)) + rnorm(n)

# a
par(mfrow = c(1, 1))
fit.a <- lm(yy.a ~ xx)
plot(yy.a ~ xx, main = "yy.a vs xx")
abline(fit.a, col = "red")
par(mfrow = c(2,2))
plot(fit.a, which = 1)
plot(fit.a, which = 2)
plot(fit.a, which = 3)
plot(fit.a, which = 4)
abline(h = .5, col = "gray")
abline(h = 1, col = "gray")
summary(fit.a)

#b
par(mfrow = c(1, 1))
fit.b <- lm(yy.b ~ xx)
plot(yy.b ~ xx, main = "yy.b vs xx")
abline(fit.b, col = "red")
par(mfrow = c(2,2))
plot(fit.b, which = 1)
plot(fit.b, which = 2)
plot(fit.b, which = 3)
plot(fit.b, which = 4)
abline(h = .5, col = "gray")
abline(h = 1, col = "gray")
summary(fit.b)

# c
par(mfrow = c(1, 1))
fit.c <- lm(yy.c ~ xx)
plot(yy.c ~ xx, main = "yy.c vs xx")
abline(fit.c, col = "red")
par(mfrow = c(2,2))
plot(fit.c, which = 1)
plot(fit.c, which = 2)
plot(fit.c, which = 3)
plot(fit.c, which = 4)
abline(h = .5, col = "gray")
abline(h = 1, col = "gray")
summary(fit.c)

# d
par(mfrow = c(1, 1))
fit.d <- lm(yy.d ~ xx)
plot(yy.d ~ xx, main = "yy.d vs xx")
abline(fit.d, col = "red")
par(mfrow = c(2,2))
plot(fit.d, which = 1)
plot(fit.d, which = 2)
plot(fit.d, which = 3)
plot(fit.d, which = 4)
abline(h = .5, col = "gray")
abline(h = 1, col = "gray")
summary(fit.d)

source("resplot.R")
resplot(fit.a)
resplot(fit.b)
resplot(fit.c)
resplot(fit.d)

# 2
load("savings.rda")
View(savings)
str(savings)
fit.savings <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)
summary(fit.savings)
resplot(fit.savings)
sort(hatvalues(fit.savings), decreasing = TRUE)[1:3]

weli <- which(rownames(savings) %in% c("Libya", "United States", "Japan"))
farb <- rep(1,nrow(savings))
farb[weli] <- c(2,3,4)
pairs(savings[,-1], pch=19, col=farb) ## Japan (red), USA (green), Libya (blue)

fit.savings.c <- lm(sr ~ pop15 + pop75 + dpi + ddpi, 
                    data = savings["Libya" != rownames(savings),])
summary(fit.savings.c)
resplot(fit.savings.c)

hist(savings$sr)
hist(savings$pop15)
hist(savings$pop75)
hist(savings$dpi)
hist(savings$ddpi)

fit.savings.log <- lm(log(sr) ~ log(pop15) + log(pop75) + log(dpi) + log(ddpi),
                      data = savings)
summary(fit.savings.log)
resplot(fit.savings.log)

fit.savings.log.pred <- lm(sr ~ log(pop15) + log(pop75) + log(dpi) + log(ddpi),
                           data = savings)
summary(fit.savings.log.pred)
resplot(fit.savings.log.pred)

# winner
fit.savings.log.econ.pred <- lm(sr ~ pop15 + pop75 + log(dpi) + log(ddpi),
                           data = savings)
summary(fit.savings.log.econ.pred)
resplot(fit.savings.log.econ.pred)

fit.savings.log.response.log.econ.pred <- lm(log(sr) ~ pop15 + pop75 + log(dpi) + log(ddpi),
                                             data = savings)
summary(fit.savings.log.response.log.econ.pred)
resplot(fit.savings.log.response.log.econ.pred)

fit.savings.log.response <- lm(log(sr) ~ pop15 + pop75 + dpi + ddpi, data = savings)
summary(fit.savings.log.response)
resplot(fit.savings.log.response)

# 3
load("synthetisch.rda")
library("car")
library("rgl")
View(synthetisch)
par(mfrow = c(3,1))
hist(synthetisch$y)
hist(synthetisch$x1)
hist(synthetisch$x2)
fit.3 <- lm(y ~ x1 + x2, data = synthetisch)
summary(fit.3)
resplot(fit.3)
par(mfrow = c(1, 1))
scatter3d(formula = y ~ x1 + x2, data = synthetisch)
options(rgl.printRglwidget = TRUE)
