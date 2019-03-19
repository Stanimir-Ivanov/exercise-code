source("resplot.R")
library(ellipse)
library(car)
library(faraway)
library(MASS)
library(glmnet)
#1
# Load and prelim analysis
load("fitness.rda")
str(fitness)

par(mfrow=c(2, 4))
histout.log <- apply(fitness, 2, FUN = function(x) hist(log(x)))
par(mfrow=c(2, 4))
histout <- apply(fitness, 2, FUN = function(x) hist(x))

par(mfrow=c(1,1))
plotcorr(cor(fitness[,-3]))

# fits
fit1 <- lm(oxy ~., data = fitness)
resplot(fit1)
crPlots(fit1)
sumary(fit1)

# log response omitting maxpulse without multicolinearity
fit3 <- lm(oxy ~ 
             age + 
             weight + 
             runtime + 
             runpulse +
             rstpulse,
           data = fitness)

summary(fit3)
resplot(fit3)
crPlots(fit3)
vif(fit3)

# add a variable to get rid of the correlation between 
# maxpulse and runpulse
my.fitness <- fitness[, -7]
my.fitness$intensity <- fitness$runpulse/fitness$maxpulse
# log response with transformed variable and no multicolinearity
fit4 <- lm(oxy ~ ., data = my.fitness)
summary(fit4)
resplot(fit4)
crPlots(fit4)
vif(fit4)

# ridge
fit.r <- lm.ridge(oxy ~ ., data = fitness, 
                  lambda = seq(from = 0, to = 5, by = .1))
select(fit.r)
par(mfrow=c(1,1))
matplot(fit.r$lambda, t(fit.r$coef), lty=1, type = "l")
legend("bottomright", legend = rownames(fit.r$coef), col = 1: dim(fit.r$coef)[1], 
       lty = 1, bg= ("white"), horiz=F)
abline(v = .5, lty = 2)
fit.r <- lm.ridge(oxy ~., data = fitness, lambda = .5)
fit.r
# backwards elimination
drop1(fit4, test = "F")
fit4a <- update(fit4, .~.-rstpulse)
drop1(fit4a, test = "F")
fit4b <- update(fit4a, .~.-runpulse)
drop1(fit4b, test = "F")
fit4c <- update(fit4b, .~.-weight)
drop1(fit4c, test = "F")
fit4d <- update(fit4c, .~.-age)
drop1(fit4d, test = "F")

summary(fit4d)
resplot(fit4d)
crPlots(fit4d)

#2
load("senic.rda")
View(senic)
str(senic)


fit.s <- lm(length ~ age + inf + region + beds + pat + nurs, 
            data = senic)
resplot(fit.s)
summary(fit.s)
vif(fit.s)

senic$ppbed <- senic$pat / senic$beds
senic$ppnurs <- senic$pat / senic$nurs

par(mfrow=c(2,2))
regions <- levels(senic$region)
vars <- c("age", "inf", "ppbed", "ppnurs", "pat")
# vars <- c("age", "inf", "beds", "pat", "nurs")
sapply(regions, FUN = function(x)
  plotcorr(cor(senic[senic$region==x, vars]), main = x, mar = c(1,1,1,1))  
)

par(mfrow=(c(2,3)))
sapply(vars, FUN = function(x)
  hist(senic[, x], main = x)
)
hist(senic$length)

fit.s2 <- lm(log(length) ~ age + inf + region + ppbed + log(ppnurs) + log(pat),
             data = senic)
summary(fit.s2)
vif(fit.s2)
resplot(fit.s2)
crPlots(fit.s2)

drop1(fit.s2, test = "F")
fit.s2.a <- update(fit.s2, .~.-ppbed)
drop1(fit.s2.a, test = "F")
summary(fit.s2.a)
resplot(fit.s2.a)
crPlots(fit.s2.a)

fit.s2.back <- step(fit.s2, direction = "backward", k = 2)

fit.null = lm(log(length) ~ 1, data = senic)
fit.full = fit.s2
scope = list(lower=fit.null, upper=fit.full)

fit.s2.forw <- step(object = fit.null,
                       scope = scope,
                       direction = "forward", k = 2)
summary(fit.s2.forw)
summary(fit.s2.back)

fit.s2.lr.stepwise <- step(object = fit.null,
                       scope = scope,
                       direction = "both", k = 2)
# 3
load("FoHF.rda")
str(FoHF)

fit.f <- lm(FoHF ~ RV + CA + FIA + EMN + ED + DS + MA + LSE + GM + EM + CTA + SS,
   data=FoHF)

summary(fit.f)
resplot(fit.f)
par(mfrow=c(1,1))
plotcorr(cor(FoHF[,-1]), mar = c(rep(1, 4)))
vif(fit.f)
crPlots(fit.f, layout = c(3,3))

par(mfrow=c(3,4))
sapply(names(FoHF), FUN = function(x) hist(FoHF[, x], main = x))

fit.f.null <- lm(FoHF ~ 1, data = FoHF)
fit.f.full <- fit.f
scope = list(lower=fit.f.null, upper=fit.f.full)
fit.f.forw <- step(object = fit.f.null,
                       scope = scope,
                       direction = "forward", k = log(nrow(FoHF)))
summary(fit.f.forw)
resplot(fit.f.forw)

fit.f.back <- step(object = fit.f.full,
                   scope = scope,
                   direction = "backward", k = log(nrow(FoHF)))
summary(fit.f.back)
resplot(fit.f.back)

fit.f.both <- step(object = fit.f.full,
                   scope = scope,
                   direction = "both", k = 2)
summary(fit.f.both)
resplot(fit.f.both)
# Lasso
xx <- model.matrix(FoHF~., data = FoHF)
yy <- FoHF$FoHF
cvfit <- cv.glmnet(xx, yy)
plot(cvfit, xvar = "lambda")
coef(cvfit, s = "lambda.1se")

fit.f.lasso <- glmnet(xx, yy)
par(mfrow=c(1,1))
plot(fit.f.lasso, xvar = "lambda")
abline(v=log(cvfit$lambda.1se))
legend("bottomright", legend = rownames(fit.f.lasso$beta), 
       col = 1: dim(fit.f.lasso$beta)[1], 
       lty = 1, bg= ("white"), ncol=2)

coef(fit.f.lasso)[,which(fit.f.lasso$lambda == cvfit$lambda.min)]

