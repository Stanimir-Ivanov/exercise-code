library(stats)

analyze_regression <- function(fit, data1, title){
  plot(data1, main = title)
  print(summary(fit))
  abline(fit, col = "red")
  plot(fit, which = 1)
  plot(fit, which = 2)
  plot(fit$fitted.values, fit$residuals^2, xlab = "Fitted", ylab = "Residuals^2", 
       main = "Residuals^2 vs. Fitted")
  lines(loess.smooth(fit$fitted.values, fit$residuals^2), col = "red")
  abline(a = summary(fit)$sigma^2, b = 0)
  acf(fit$residuals, type = "correlation")
}

## Q1
load("conconi.rda")
analyze_regression(lm(puls ~ speed, data = conconi), conconi, 
                   title = "Conconi test: Pulse vs. speed")
## Q2
# a
load("gas.rda")
analyze_regression(lm(verbrauch ~ temp, data = gas), gas, 
                   title =  "Gas usage vs. temperature")
# b
load("antikeUhren.rda")
analyze_regression(lm(Preis ~ Alter, data = antikeUhren), antikeUhren, 
                   title =  "Preis vs. Alter")
## Q4
highwayRunoff <- read.csv("highwayRunoff.csv", header = TRUE)
fit1 <- lm(runoff ~ rainfall, data = highwayRunoff)
analyze_regression(fit1, highwayRunoff, 
                   title =  "Rainfall vs. Runoff")
predictValues1 <- data.frame(50)
colnames(predictValues1) <- "rainfall"

pred1 <- predict.lm(object = fit1, newdata = predictValues1, se.fit = TRUE, 
           interval = "prediction", level = 0.95)


hist(highwayRunoff$rainfall)
hist(highwayRunoff$runoff)
# log transform model
fit2 <- lm(log(runoff) ~ log(rainfall), data = highwayRunoff)
analyze_regression(fit2, data = log(highwayRunoff), 
                   title =  "log(Rainfall) vs. log(Runoff)")

predictValues2 <- data.frame(50)
colnames(predictValues2) <- "rainfall"

pred2 <- predict.lm(object = fit2, newdata = predictValues2, se.fit = TRUE, 
           interval = "prediction", level = 0.95)

pred_interval <- data.frame(predict.lm(object = fit2, interval = "prediction"))
# plot on log scale
plot(log(highwayRunoff))
abline(fit2, col = "red")
lines(cbind(log(highwayRunoff$rainfall), pred_interval$lwr))
lines(cbind(log(highwayRunoff$rainfall), pred_interval$upr))
# plot on original scale
plot(highwayRunoff, main = "Highway Runoff")
xx <- data.frame(rainfall=0:150)
yy <- predict(fit2, newdata=xx, interval="confidence")
lines(xx$rainfall, exp(yy[,"fit"]), col="red")
lines(xx$rainfall, exp(yy[,"lwr"]), col="blue")
lines(xx$rainfall, exp(yy[,"upr"]), col="blue")
yy <- predict(fit2, newdata=xx, interval="prediction")
lines(xx$rainfall, exp(yy[,"lwr"]), col="darkgreen")
lines(xx$rainfall, exp(yy[,"upr"]), col="darkgreen")

# rejected model
analyze_regression(lm(runoff ~ log(rainfall), data = highwayRunoff), cbind(log(highwayRunoff$rainfall), highwayRunoff$runoff), 
                   title =  "Rainfall vs. log(Runoff)")
#Q5
bacteria <- read.csv("bacteria.csv", header = TRUE)
# plot(bacteria)
analyze_regression(lm(Amount ~ Interval, data = bacteria), bacteria, 
                   title =  "Surviving bacteria vs. interval")

# plot(cbind(bacteria$Interval, log(bacteria$Amount)))
fit3 <- lm(log(Amount) ~ Interval, data = bacteria)
analyze_regression(fit3, data1 = cbind(bacteria$Interval, log(bacteria$Amount)),
                   title = "Surviving log(bacteria) vs. interval")
exp(coef(fit3)[2])
predictValues3 <- data.frame(5)
colnames(predictValues3) <- "Interval"

pred3 <- predict.lm(fit3, predictValues3, interval = "prediction")
exp(pred3)
exp(pred3 + (summary(fit3)$sigma^2)/2)[1]

predictValues4 <- data.frame(0)
colnames(predictValues4) <- "Interval"

pred3 <- predict.lm(fit3, predictValues4, interval = "prediction")

