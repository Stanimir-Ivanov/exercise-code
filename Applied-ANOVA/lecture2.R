data("PlantGrowth")
View(PlantGrowth)
levels(PlantGrowth$group)
boxplot(weight ~ group, data = PlantGrowth)
options(contrasts = c("contr.treatment", "contr.poly"))
fit <- aov(weight ~ group , data = PlantGrowth) # analysis of variance
coef(fit)
dummy.coef(fit)
predict(fit, newdata = data.frame(group = c("ctrl", "trt1", "trt2")))
options(contrasts = c("contr.sum", "contr.poly"))
fit2 <- aov(weight ~ group, data = PlantGrowth)
coef(fit2)
dummy.coef(fit2)
predict(fit2, newdata = data.frame(group = c("ctrl", "trt1", "trt2")))

summary.lm(fit)

plot(fit, which = 1) # QQ
plot(fit, which = 2) # TA
