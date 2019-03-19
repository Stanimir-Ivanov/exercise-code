# Q1
load("catheter.rda")
hist(catheter$height)
hist(catheter$weight)
hist(catheter$catlength)
pairs(catheter)

fit.h <- lm(catlength ~ height, data = catheter)
fit.w <- lm(catlength ~ weight, data = catheter)
summary(fit.h)
summary(fit.w)

fit.m <- lm(catlength ~ height + weight, data = catheter)
summary(fit.m)
# predict catlength for a child that is 120cm tall and has a weight of 25kg
new.d <- data.frame(height = 120, weight = 25)
pred.w <- predict(fit.w, newdata = new.d, interval = "prediction")
pred.h <- predict(fit.h, newdata = new.d, interval = "prediction")
pred.m <- predict(fit.m, newdata = new.d, interval = "prediction")
# Q2
x1 = 3.5
x2 = 1
cake.med <- exp(-.4150 + 4.0609 * x1 - 1.0725 * x1 ^ 2 + 2.0109 * x2)
cake.mean <- exp(-.4150 + 4.0609 * x1 - 1.0725 * x1 ^ 2 + 2.0109 * x2 + 2.784^2/2)

test.stat <- ((46*2.784^2 - 45*2.7^2)/(45*2.7^2))*(45/1)
1- pf(test.stat, df1 = 1, df2 = 45)

# Q3
load("conconi2.rda")
str(conconi2)
dani <- cbind("Dani", conconi2$Speed[!is.na(conconi2$Dani.Puls)], conconi2$Dani.Puls[!is.na(conconi2$Dani.Puls)])
marcel <- cbind("Marcel", conconi2$Speed[!is.na(conconi2$Marcel.Puls)], conconi2$Marcel.Puls[!is.na(conconi2$Marcel.Puls)])
conconi.rshp <- data.frame(rbind(dani, marcel))
colnames(conconi.rshp) <- c("runner", "speed", "puls")
conconi.rshp$speed <- as.numeric(levels(conconi.rshp$speed))[conconi.rshp$speed]
conconi.rshp$puls <- as.numeric(levels(conconi.rshp$puls))[conconi.rshp$puls]
str(conconi.rshp)
fit.conconi <- lm(formula = puls ~ speed + runner, data = conconi.rshp)
summary(fit.conconi)
plot(fit.conconi, which = 1)
plot(fit.conconi, which = 2)
plot(fit.conconi$residuals, col = as.numeric(conconi.rshp$runner))
abline(a = 0, b = 0)

fit.conconi2 <- lm(formula = puls ~ speed * runner, data = conconi.rshp)
summary(fit.conconi2)
plot(fit.conconi2, which = 1)
plot(fit.conconi2, which = 2)

zero.m <- data.frame(speed = 0, runner = "Marcel")
zero.d <- data.frame(speed = 0, runner = "Dani")

pred.m <- predict(fit.conconi2, newdata = zero.m, interval = "prediction")
pred.d <- predict(fit.conconi2, newdata = zero.d, interval = "prediction")

all.speed <- seq(from = 9, to = 21.5, by = .5)
all.m <- data.frame(speed = all.speed, runner = "Marcel")
all.d <- data.frame(speed = all.speed, runner = "Dani")

pred.all.m <- predict(fit.conconi2, newdata = all.m, interval = "prediction")
pred.all.d <- predict(fit.conconi2, newdata = all.d, interval = "prediction")
plot(conconi.rshp$speed, conconi.rshp$puls, col = as.numeric(conconi.rshp$runner),
     main = "Conconi test for two runners", xlab = "Speed in Km/h", ylab = "Pulse in bpm")
lines(all.speed, pred.all.d[,"fit"], col = "black")
lines(all.speed, pred.all.d[,"upr"], col = "blue")
lines(all.speed, pred.all.d[,"lwr"], col = "blue")

lines(all.speed, pred.all.m[,"fit"], col = "red")
lines(all.speed, pred.all.m[,"upr"], col = "green")
lines(all.speed, pred.all.m[,"lwr"], col = "green")

# Q4
load("farm.rda")
View(farm)
str(farm)
farm$region <- as.factor(farm$region)
farm$industry <- as.factor(farm$industry)
library(plyr)
farm$industry <- revalue(farm$industry, c("1"="wheat", 
                         "2"= "wheat, sheep, cattle", 
                         "3" = "sheep", "4" = "cattle", 
                         "5" = "sheep, cattle"))

par(mfrow=c(1,2))
plot(table(farm$region), main="Region")
plot(table(farm$industry), main="Industry")

hist(farm$ertrag)
hist(farm$aufwand)
fit.farm <- lm(log(ertrag) ~ log(aufwand) + region + industry, data = farm)
summary(fit.farm)
plot(fit.farm, which = 1)
plot(fit.farm, which = 2)

new.farm <- data.frame(industry = "cattle", aufwand = 1e5, region = "111")
pred.new.farm <- predict(fit.farm, newdata = new.farm, interval = "prediction")
exp(pred.new.farm + .5*summary(fit.farm)$sigma^2)

drop1(fit.farm, scope = "region", test="F")

fit.farm.inter <- lm(log(ertrag) ~ log(aufwand) + industry + region * industry, data = farm)
summary(fit.farm.inter)

(length(levels(farm$region)) - 1) * (length(levels(farm$industry)) - 1)

anova(fit.farm, fit.farm.inter)
