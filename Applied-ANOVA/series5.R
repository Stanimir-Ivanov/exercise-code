legierung <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/legierung.dat",
                        header = TRUE)
View(legierung)

#transform factors
factor.cols <- c("day", "temp" ,"alloy")
legierung[factor.cols] <- lapply(legierung[factor.cols], factor)
str(legierung)

par(mfrow = c(1, 2))
plot.design(legierung)
with(legierung, interaction.plot(x.factor = temp, trace.factor = alloy,
                              response = breaking))

library(lmerTest)
fit.alloy <- lmer(breaking ~ temp * alloy + day + (1 | day:temp), data = legierung)
summary(fit.alloy)
confint(fit.alloy, oldNames = FALSE)
anova(fit.alloy)

par(mfrow=c(1,1))
plot(fit.alloy)
par(mfrow = c(1, 2))
qqnorm(ranef(fit.alloy)$"day:temp"[, 1])
qqnorm(resid(fit.alloy))

#4
nitrogen <- read.table("http://stat.ethz.ch/Teaching/Datasets/nitrogen.dat",
                       header = TRUE)
View(nitrogen)

nit.factor.cols <- c("nitrogen", "block", "thatch")
nitrogen[nit.factor.cols] <- lapply(nitrogen[nit.factor.cols], factor)
str(nitrogen)

par(mfrow = c(1, 2))
plot.design(nitrogen)
with(nitrogen, interaction.plot(x.factor = thatch, trace.factor = nitrogen,
                                 response = chlorophyll))

fit.nitro <- lmer(chlorophyll ~ nitrogen * thatch + block + (1| nitrogen:block),
                  data = nitrogen)
summary(fit.nitro)
anova(fit.nitro)
confint(fit.nitro, oldNames = FALSE)


par(mfrow=c(1,1))
plot(fit.nitro)
par(mfrow = c(1, 2))
qqnorm(ranef(fit.nitro)$"nitrogen:block"[, 1])
qqnorm(resid(fit.nitro))

fit.nitro2 <- lmer(chlorophyll ~ nitrogen + thatch + block + (1| nitrogen:block),
                   data = nitrogen)
summary(fit.nitro2)$coefficients
