#1
library(lme4)
alloy <- read.table(file="http://stat.ethz.ch/Teaching/Datasets/WBL/alloy.dat",
                    header = TRUE)
alloy$casting <- as.factor(alloy$casting)
str(alloy)

boxplot(formula = strength ~ casting, data = alloy)

library(lmerTest)
fit.alloy <- lmer(strength ~ (1 | casting), data = alloy)
summary(fit.alloy)
confint(fit.alloy)
plot(fit.alloy)
# library(lattice)
# qqmath(ranef(fit.alloy))
par(mfrow = c(1, 2))
qqnorm(ranef(fit.alloy)$casting[, 1], main = "Random effects of casting")
qqnorm(resid(fit.alloy), main = "Residuals")
#2
pasteurization <- read.table(file = "http://stat.ethz.ch/Teaching/Datasets/WBL/pasteurization.dat",
                             header = TRUE)
str(pasteurization)
View(pasteurization)
pasteurization$sample <- as.factor(pasteurization$sample)
pasteurization$lab <- as.factor(pasteurization$lab)

par(mfrow=c(1,1))
with(pasteurization, interaction.plot(x.factor = sample, 
                                      trace.factor = lab,
                                      response = log(bacteria)))
# log seems to produce a better fit
fit.past <- lmer(log(bacteria) ~ (1 | sample) + (1 | lab) + (1 | lab:sample),
                 data = pasteurization)
summary(fit.past)
anova(fit.past)
confint(fit.past)

plot(fit.past)
par(mfrow=c(2,2))
qqnorm(coef(fit.past)$lab[,1], 
       main ="Normal Q-Q Plot Lab Random Effect")
qqnorm(coef(fit.past)$'sample'[,1], 
       main = "Normal Q-Q Plot Sample Random Effect")
qqnorm(coef(fit.past)$'lab:sample'[,1], 
       main = "Normal Q-Q Plot Lab:Sample Random Interaction Effect")
qqnorm(resid(fit.past),
       main = "Normal Q-Q Plot Resid")

#3
heartvalves <- read.table(file = "http://stat.ethz.ch/Teaching/Datasets/heartvalves.dat",
                          header = TRUE)
str(heartvalves)
heartvalves$type <- as.factor(heartvalves$type)
heartvalves$pulse <- as.factor(heartvalves$pulse)
heartvalves$valve <- as.factor(heartvalves$valve)

library(ggplot2)
ggplot(heartvalves, aes(x = pulse, y = flow, 
                   group = valve,
                   color = type)) + stat_summary(fun.y = mean, geom = "line")
stripchart(flow ~ pulse + valve, data = heartvalves, method = "jitter",
           vertical=TRUE, col = as.numeric(heartvalves$type))

fit.heart <- lmer(flow ~ type + pulse + (1 | valve), data = heartvalves)
summary(fit.heart)
confint(fit.heart)
anova(fit.heart)

plot(fit.heart)
par(mfrow=c(1,2))
qqnorm(resid(fit.heart))
qqnorm(coef(fit.heart)$valve[,1])
