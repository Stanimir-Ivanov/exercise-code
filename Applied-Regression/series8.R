library(mgcv)
source("resplot.R")

TA.plot.bin.model <- function(model){
  xx <- predict(model, type="response")
  yy <- residuals(model, type="deviance")
  
  plot(yy ~ xx, main = "Tukey-Anscombe 1", xlab = "fitted probabilities",
       ylab = "deviance resid")
  lines(loess.smooth(xx, yy, family="gaussian", pch=20), col = "red")
  abline(h=0, lty=3)
}


car <- read.table("http://stat.ethz.ch/Teaching/Datasets/car.dat",header=T)

plot(age ~ income, data=car, log="x", type="n")
points(age ~ income, subset=(purchase==0), data=car, pch = 7, col = "red")
points(age ~ income, subset=(purchase==1), data=car, pch = 16, col = "blue")
title("Purchase")

fit.car <- glm(purchase ~ income + age, family = binomial, data = car)
summary(fit.car)

beta.income = coefficients(fit.car)["income"]
beta.age = coefficients(fit.car)["age"]

exp(beta.income)
exp(beta.age)

c <- predict(fit.car, newdata=data.frame(age=3, income=50), type = "response")
c

TA.plot.bin.model(fit.car)
plot(fit.car, which = 5)

drop1(fit.car, test = "Chisq")

fit.car.inter <- glm(purchase ~ income + age + income*age, family = binomial, data = car)
summary(fit.car.inter)
drop1(fit.car.inter, test = "Chisq")
#2

no.yes <- c("No", "Yes")
smoking <- gl(2,1,7, no.yes)
obesity <- gl(2,2,7, no.yes)
snoring <- gl(2,4,7, no.yes)
n.total <- c(60, 17, 8, 187, 85, 51, 23)
n.hyper <- c(5, 2, 1, 35, 13, 15, 8)

hyper <- cbind(yes = n.hyper, no = n.total - n.hyper)
hyper

fit.hyper <- glm(hyper ~ smoking + obesity + snoring, family = binomial)
summary(fit.hyper)
pchisq(deviance(fit.hyper), df.residual(fit.hyper), lower = FALSE)
#global lr test
pchisq(fit.hyper$null.deviance - fit.hyper$deviance, 
       df = fit.hyper$df.null - fit.hyper$df.residual, lower = FALSE)

TA.plot.bin.model(fit.hyper)
plot(fit.hyper, which = 5)
drop1(fit.hyper, test="Chisq")
fit.hyper1 <- glm(hyper ~ obesity + snoring, family = binomial)
summary(fit.hyper1)
drop1(fit.hyper1, test="Chisq")
pchisq(fit.hyper1$deviance, df = fit.hyper1$df.residual, lower = FALSE)

TA.plot.bin.model(fit.hyper1)

prop <- data.frame(fitted=round(fitted(fit.hyper1),2), observed=round(n.hyper/n.total, 2))
count <- data.frame(fitted=n.total*prop$fitted, observed = n.hyper)

#3
nematodes <- read.csv("nematodes.csv", header=TRUE)
nematodes$Sample <- as.factor(nematodes$Sample)
nematodes$Volume <- as.factor(nematodes$Volume)

fit.nem <- glm(Nematodes ~ Sample, family=poisson, data=nematodes)
summary(fit.nem)
# fit.nem.gam <- gam(Nematodes ~ Sample, family=poisson, data=nematodes)
# summary(fit.nem.gam)
drop1(fit.nem, test="Chisq")

TA.plot.bin.model(fit.nem)
plot(fit.nem, which = 5)

# need numeric volume
nematodes <- read.csv("nematodes.csv", header=TRUE)
nematodes$Sample <- as.factor(nematodes$Sample)

fit.nem.lr <- glm(Nematodes ~ log(Volume), family=poisson, data=nematodes)
summary(fit.nem.lr)
pchisq(fit.nem.lr$deviance, df = fit.nem.lr$df.residual, lower = FALSE)
confint(fit.nem.lr)

fit.nem.lr.offset <- glm(Nematodes ~ offset(log(Volume)), family = poisson, data = nematodes)
summary(fit.nem.lr.offset)
pchisq(fit.nem.lr.offset$deviance, df = fit.nem.lr.offset$df.residual, lower = FALSE)

#4
library(foreign)
pension <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge2k/pension.dta")
View(pension)
head(pension)
str(pension)
#delete NAs
pension <- pension[!apply(is.na(pension), MARGIN=1,any),]

# label pchtstck=0 as reference
pension$pctstck[pension$pctstck==50] <- 1
pension$pctstck[pension$pctstck==100] <- 2

cols <- c("prftshr", "choice", "female", "married", "finc25", "finc35", "finc50", 
          "finc75", "finc100", "finc101", "black", "stckin89", "irain89")
pension[cols] <- lapply(pension[cols], factor)  ## as.factor() could also be used

table(pension$choice,pension$pctstck)

mosaicplot(table(pension$choice,pension$pctstck), color=TRUE,
           main="choice vs. pctstck",xlab="choice",ylab="pctstck")

# optional 
fit.pens1 <- gam(list(pctstck ~ choice, ~ choice), 
                 data = pension, family = multinom(K=2))
summary(fit.pens1)

#c
pension <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge2k/pension.dta")

# make income variable
pension$inc <- NA
pension$inc[pension$finc25==1] <- 1
pension$inc[pension$finc35 | pension$finc50] <- 2
pension$inc[pension$finc75 | pension$finc100 | pension$finc101] <- 3
pension$inc <- factor(pension$inc,labels=
                        c("below 25'000","25'000 to 50'000", "above 50'000"))

# make factors
cols <- c("pctstck", "prftshr", "choice", "female", "married", "black")
pension[cols] <- lapply(pension[cols], factor)  ## as.factor() could also be used
pension$pct <- factor(pension$pctstck, levels = c("50","0","100"),
                      ordered = FALSE)

table(pension$inc,pension$pctstck)

mosaicplot(table(pension$inc,pension$pctstck), color=TRUE,
           main="income vs. pctstck",xlab="income",ylab="pctstck")

pension$age <- sqrt(pension$age)
pension$educ <- sqrt(pension$educ)

library(nnet)

mod1 <- multinom(pct~choice+age+educ+female+married+black+inc
                 +wealth89+prftshr, data=pension)
summary(mod1)

mod2 <- multinom(pct~age+educ+female+married+black+inc+wealth89+prftshr, data=pension)
summary(mod2)
pchisq(deviance(mod2) - deviance(mod1), mod1$edf - mod2$edf, lower=FALSE)

predict(mod1,type="probs",newdata=data.frame(choice="0",age=
                                               sqrt(60), educ=sqrt(13.5),
                                             female="0",married="0",
                                             black="0",inc="above 50'000",
                                             wealth89=200,prftshr="1"))
predict(mod1,type="probs",newdata=data.frame(choice="1",age=
                                               sqrt(60), educ= sqrt(13.5),
                                             female="0",married="0",
                                             black="0",inc="above 50'000",
                                             wealth89=200,prftshr="1"))
