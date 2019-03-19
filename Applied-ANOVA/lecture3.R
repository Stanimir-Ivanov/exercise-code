library(multcomp)
data("PlantGrowth")
fit <- aov(weight ~ group , data = PlantGrowth) # analysis of variance
fit.gh <- glht(fit, linfct = mcp(group = c(1, -1/2, -1/2)))
summary(fit.gh)
confint(fit.gh)

## Create a matrix where each *row* is a contrast
K <- rbind(c(1, -1/2, -1/2), ## ctrl vs. average of trt1 and trt2
           c(1, -1, 0)) ## ctrl vs. trt1
fit.gh <- glht(fit, linfct = mcp(group = K))
## Individual p-values
summary(fit.gh, test = adjusted("none"))
## Bonferroni corrected p-values
summary(fit.gh, test = adjusted("bonferroni"))
summary(fit.gh, test = adjusted("holm"))

## p-value according to Scheffe (g = 3, N - g = 27)
fit.scheffe <- glht(fit, linfct = mcp(group = c(1/2, -1, 1/2)))
pf((summary(fit.scheffe)$test$tstat)^2 / 2, 2, 27, lower.tail = FALSE)

## Without correction (but pooled sd estimate)
pairwise.t.test(PlantGrowth$weight, PlantGrowth$group, p.adjust.method = "none")

## With correction (and pooled sd estimate)
pairwise.t.test(PlantGrowth$weight, PlantGrowth$group, p.adjust.method = "holm")

## Tukey HSD with built-in function, including plots:
TukeyHSD(fit)
plot(TukeyHSD(fit))

fit.tukey <- glht(fit, linfct = mcp(group = "Tukey"))
summary(fit.tukey)
confint(fit.tukey)
plot(confint(fit.tukey))


fit.dunnett <- glht(fit, linfct = mcp(group = "Dunnett"))
summary(fit.dunnett)

# Appendix
x <- factor(rep(c("A", "B", "C"), each = 2))
y1 <- c(0.50, 0.62,
        0.46, 0.63,
        0.95, 0.86)
y2 <- c(0.23, 0.34,
        0.45, 0.55,
        0.55, 0.66)
stripchart(y1 ~ x, vertical = TRUE, pch = 1)
fit1 <- aov(y1 ~ x)
summary(fit1)

TukeyHSD(fit1)

fit2 <- aov(y2 ~ x)
summary(fit2)

TukeyHSD(fit2)
