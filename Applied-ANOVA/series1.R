# Q3
c(rep("A", 10), rep("B", 10))
rep(c("A", "B"), 10)
rbern(20, .5)
rbinom(10, 1, 0.5)
sample(rep(c("A", "B"), 10), replace = FALSE)

# Q4
# data
d.stream <- read.csv("znk.csv", sep = " ", header = TRUE)
# a
str(d.stream)
d.stream$ZINC <- factor(d.stream$ZINC, levels = c("BACK", "LOW", "MED", "HIGH"))
plot(DIVERSITY ~ ZINC, data = d.stream)
stripchart(DIVERSITY ~ ZINC, data = d.stream, vertical = TRUE)
# b
fit <- aov(DIVERSITY ~ ZINC, data = d.stream)
summary(fit)
# c
plot(fit, which = 1) # Tukey-Anscombe plot
plot(fit, which = 2) # QQ norm plot
# d
coef(fit)
summary.lm(fit)
dummy.coef(fit)
#Q5
cgl <- read.csv("cgl.csv", sep = "\t", header = TRUE)
# a
plot(time ~ trmt, data = cgl)
stripchart(time ~ trmt, data = cgl, method = "stack", vertical = TRUE)
#b
g <- length(levels(cgl$trmt))
ms_e <- sum(tapply(cgl$time, cgl$trmt, FUN = function(x){
  sum((x - mean(x))^2)
}))/(length(cgl$time) - g)
#c
grand_mean <- mean(cgl$time) # overall mean
group_means <- tapply(cgl$time, cgl$trmt, mean) # mean by factor
aggregate(time ~ trmt, data = cgl, mean) # a better way
group_sizes <- tapply(cgl$time, cgl$trmt, length)
ms_trt <- group_sizes %*% (group_means - grand_mean)^2 / (g - 1) ## MS Trt
#d
fit2 <- aov(time ~ trmt, data = cgl)
#e 
summary(fit2) #ya boi
#f
plot(fit2, which = 1)
plot(fit2, which = 2)
