library(car)
library(MASS)
library(glmnet) # complexity regularization
library(relaimpo) # relative importance
library(mgcv) # generalized additive models
source("resplot.R")

# 1
load("CustomerWinBack.rda")
View(cwb)

cwb$gender <- as.factor(cwb$gender)

# explore data
par(mfrow=c(2,3))
vars  <- colnames(cwb)
sapply(vars, FUN = function(x) {
  hist(cwb[, x], main = x)
})

# full fits
fit1 <- lm(duration ~ ., data = cwb)
resplot(fit1)
summary(fit1)
hist(fit1$residuals)

crPlots(fit1, layout=c(2,3), cex.lab = .75)

fit.full = fit1
fit.null = lm(duration  ~ 1, data = cwb)
scope = list(lower=fit.null, upper=fit.full)

# aic, k = 2
fit.aic <- step(fit.full, scope = scope, direction = "both", k = 2)
summary(fit.aic)
# bic, k = log(n)
fit.bic <- step(fit.full, scope = scope, direction = "both", k = log(nrow(cwb)))
summary(fit.bic)

fit.rdg <- lm.ridge(duration ~ ., data = cwb, lambda = seq(15.07, 15.08, 1e-4))
select(fit.rdg)
fit.rdg <- lm.ridge(duration ~ ., data = cwb, lambda = 15.0728)
coef(fit.rdg)

## Lasso does not work with factor variables
xx <- model.matrix(duration ~ 0 + ., cwb)[,-4]
yy <- cwb$duration
fit.lasso <- glmnet(xx,yy)
cvfit <- cv.glmnet(xx,yy)
coef(cvfit)

# CV
pre.ols <- c()
pre.aic <- c()
pre.bic <- c()
pre.rr <- c()
pre.las <- c()

folds <- 5
sb <- round(seq(0,nrow(cwb),length=(folds+1)))

for (i in 1:folds)
{
  ## define training and test datasets
  test <- (sb[((folds+1)-i)]+1):(sb[((folds+2)-i)])
  train <- (1:nrow(cwb))[-test]
  ## fit models
  fit.ols <- lm(duration ~ ., data=cwb[train,])
  fit.aic <- step(fit.ols)
  fit.bic <- step(fit.ols, k=log(nrow(cwb[train,])))
  fit.rc <- lm.ridge(duration ~ ., lambda=seq(0,100,0.1), data=cwb[train,])
  lambda <- as.numeric(attributes(which.min(fit.rc$GCV))$names)
  fit.rr <- lm.ridge(duration ~ ., lambda=lambda, data=cwb[train,])
  ## Lasso does not work with factor variables
  xx <- model.matrix(duration~0+., cwb[train,])[,-4]
  yy <- cwb$duration[train]
  fit.las <- cv.glmnet(xx,yy)
  ##create predictions
  pre.ols[test] <- predict(fit.ols, newdata=cwb[test,])
  pre.aic[test] <- predict(fit.aic, newdata=cwb[test,])
  pre.bic[test] <- predict(fit.bic, newdata=cwb[test,])
  pre.rr[test] <- model.matrix(duration~., cwb[test,])%*%coef(fit.rr)
  pre.las[test] <- model.matrix(duration~., cwb[test,])%*%as.numeric(coef(fit.las))
}

mean((cwb$duration-pre.ols)^2)
mean((cwb$duration-pre.aic)^2)
mean((cwb$duration-pre.bic)^2)
mean((cwb$duration-pre.rr)^2)
mean((cwb$duration-pre.las)^2)

# AIC is best, assess relevance 
maxmin <- function(col) max(col) - min(col)
ranges <- apply(cwb[,c(2:4)], 2, maxmin)
max.rele <- c(abs(ranges*coef(fit.aic)[c(2:4)]), abs(coef(fit.aic)[5]))

out.betasq <- calc.relimp(fit.aic, type="betasq", rela=TRUE)
out.lmg <- calc.relimp(fit.aic, type="lmg", rela=TRUE)

pval <- (summary(fit.aic)$coefficients[,4])[-1]

par(mfrow=c(2,2))

plot(-log10(pval), max.rele, pch=20, main="Maximal effect")
plot(-log10(pval), out.betasq@betasq, pch=20, main="Beta Sq")
plot(-log10(pval), out.lmg@lmg, pch=20, main="LMG")

par(mfrow=c(1,1))

#2
load("autodistanz.rda")
View(autodistanz)

plot(autodistanz)
smth <- with(autodistanz, loess.smooth(x=alter, y=distanz, kernel="normal", span=.3))
lines(smth, col="red")

fit.poly <- lm(distanz ~ poly(alter, 4), data=autodistanz)
newdata <- data.frame(alter=seq(min(autodistanz$alter), max(autodistanz$alter), .01))
newdata$distanz <- predict(fit.poly, newdata)
lines(x=newdata$alter, y=newdata$distanz, col='yellow') 

summary(fit.poly)
resplot(fit.poly)

# need a log transform due to non-constant scatter
par(mfrow=c(1,1))
plot(log(distanz) ~ alter, data=autodistanz)
smth <- with(autodistanz, loess.smooth(x=alter, y=log(distanz), kernel="normal", span=.3))
lines(smth, col="red")

fit.poly <- lm(log(distanz) ~ poly(alter, 4), data=autodistanz)
newdata <- data.frame(alter=seq(min(autodistanz$alter), max(autodistanz$alter), .01))
newdata$distanz <- predict(fit.poly, newdata)
lines(x=newdata$alter, y=newdata$distanz, col='blue') 

summary(fit.poly)
resplot(fit.poly)

fit.spln <- with(autodistanz, smooth.spline(alter, distanz, all.knots=TRUE))
par(mfrow=c(1,1))
plot(distanz ~ alter, data = autodistanz)
lines(fit.spln, col = "red")
fit.spln

fit.gam <- gam(distanz ~ s(alter), data = autodistanz)
newdata1 <- data.frame(alter=seq(from = min(autodistanz$alter), to = max(autodistanz$alter), by = .1))
lines(x = newdata1$alter, y = predict(fit.gam, newdata = newdata1), col = "red")

## generate and visualize the basis
par(mfrow = c(1,1))
library(splines)
range(autodistanz$alter)
xx <- sort(autodistanz$alter)
yy <- autodistanz$distanz[order(autodistanz$alter)]
kn <- c(18,18,18,18,35,83,83,83,83)
bx <- splineDesign (kn, xx)

fit.spline <- lm (yy ~ 0+bx)
summary(fit.spline)

par(mfrow = c(1,2))
matplot(xx, bx, type="l")
matplot(xx, cbind (yy, fit.spline$fitted), type="pl", pch=20, xlab="Age", ylab="Distance")

load("no2basel.rda")
View(no2basel)

par(mfrow=c(2,2))
cls <- colnames(no2basel)
sapply(cls, FUN = function(x){
  hist(no2basel[,x], main=x)
})

fit.no2 <- lm(log(NO2) ~ Tag + Temp + Wind, data = no2basel)
summary(fit.no2)
resplot(fit.no2)
par(mfrow=c(2,2))
crPlots(fit.no2, cex.lab = .75)

par(mfrow=c(2,2))
fit.no2.g <- gam(NO2 ~ s(Tag) + s(Temp) + s(Wind), data = no2basel)
plot(fit.no2.g, shade=TRUE, residuals=TRUE, pch=20, main="GAM Partial Residual Plots")

par(mfrow=c(2,2))
fit.no2.gl <- gam(log(NO2) ~ s(Tag) + s(Temp) + s(Wind), data = no2basel)
plot(fit.no2.gl, shade=TRUE, residuals=TRUE, pch=20, main="GAM Partial Residual Plots of Logged Response Model")

par(mfrow=c(2,2))
gam.check(fit.no2.g, pch=20, rep=100)
gam.check(fit.no2.gl, pch=20, rep=100)

fit.no2.f <- gam(log(NO2) ~ s(Tag) + Temp + Wind, data=no2basel)
par(mfrow=c(2,2))
plot(fit.no2.f, shade=TRUE, residuals=TRUE, pch=20, main="GAM Partial Residual Plots")
par(mfrow=c(2,2))
gam.check(fit.no2.f, pch=20, rep=100)

summary(fit.no2.f)

fit.ols <- gam(log(NO2) ~ Tag + Temp + Wind, data=no2basel)
anova(fit.ols, fit.no2.f, test="Chisq")
anova(fit.no2.f, fit.no2.gl, test = "Chisq")

data("Prestige")
fit.pres <- gam(prestige ~ s(income) + s(education), data = Prestige)
par(mfrow=c(2,1))
plot(fit.pres, shade=TRUE, residuals=TRUE, pch=20, main="GAM Partial Residual Plots")
gam.check(fit.pres, pch=20, rep=100)
summary(fit.pres)

fit.pres.log.pr <- gam(prestige ~ s(log(income)) + s(education),
                                                     data = Prestige)
plot(fit.pres.log.pr, shade=TRUE, residuals=TRUE, pch=20, main="GAM Partial Residual Plots")
gam.check(fit.pres, pch=20, rep=100)

fit.pres.ols <- lm(prestige ~ income + education, data = Prestige)
resplot(fit.pres.ols)
crPlots(fit.pres.ols, layout=c(2,1), cex.lab = .75)
fit.pres.ols <- lm(prestige ~ income + education, data = Prestige)

fit.pres.ols.l <- lm(prestige ~ log(income) + education, data = Prestige)
resplot(fit.pres.ols.l)
crPlots(fit.pres.ols.l, layout=c(2,1), cex.lab = .75)
summary(fit.pres.ols.l)
summary(fit.pres.log.pr)
anova(fit.pres.ols.l, fit.pres.log.pr)

Prestige$uni <- factor(Prestige$education>12, labels=c("nein", "ja"))
fit.pres.3f <- gam(prestige ~ s(log(income)) + s(education) + uni, data=Prestige)
summary(fit.pres.3f)
par(mfrow=c(2,1))
plot(fit.pres.3f, shade=TRUE, residuals=TRUE, pch=20, main="GAM Partial Residual Plots")



