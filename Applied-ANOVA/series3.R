# 2
library(car)
dat <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/unbalanced.dat",
                     header = TRUE)

options(contrasts = c("contr.sum", "contr.sum"))
dat$A <- factor(dat$A)
dat$B <- factor(dat$B)
fit <- aov(Y ~ A * B, data = dat)
summary(fit)
Anova(fit, type = "II")
Anova(fit, type = "III")
drop1(fit, scope = ~., test = "F")

fit.null <- aov(Y ~ 1, data = dat)
fit.main <- aov(Y ~ A + B, data = dat)
anova(fit.null, fit.main)

Anova(fit.main, type = "II")

fit.A <- aov(Y ~ A + B, data = dat)
anova(fit.A)
fit.B <- aov(Y ~ B + A, data = dat)
anova(fit.B)

#3
coffee <- read.table(file = "http://stat.ethz.ch/Teaching/Datasets/WBL/coffee.dat",
                     header = TRUE)
coffee$location <- factor(coffee$location)
coffee$density  <- factor(coffee$density)
with(coffee, interaction.plot(x.factor = density, trace.factor = location,
                              response = coffee))

fit.coffee <- aov(coffee ~ density + location, data = coffee)
summary(fit.coffee)

par(mfrow = c(1, 2))
plot(fit.coffee, which = 1:2)

coffee.fit2 <- aov(log(coffee) ~ density + location, data = coffee)
par(mfrow = c(1, 2))
plot(coffee.fit2, which = 1:2)
