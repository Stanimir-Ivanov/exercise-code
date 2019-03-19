## Create data (skip if not interested) ####
acids <- c(1.697, 1.601, 1.830,
           2.032, 2.017, 2.409,
           2.211, 1.673, 1.973,
           2.091, 2.255, 2.987)
R50 <- rep(c("no", "yes", "no", "yes"), each = 3)
R21 <- rep(c("no", "no", "yes", "yes"), each = 3)
cheddar <- data.frame(R50, R21, acids)
# count the number of observations for every combination of
# the levels of thetwo factors
xtabs(~ R50 + R21, data = cheddar)
## elegant way, using the function "with"
with(cheddar, interaction.plot(x.factor = R50, trace.factor = R21, response = acids))
## standard way: interaction.plot(x.factor = cheddar$R50, trace.factor = cheddar$R21,
## response = cheddar$acids)
options(contrasts = c("contr.sum", "contr.poly"))
fit.cheddar <- aov(acids ~ R50 * R21, data = cheddar)
coef(fit.cheddar)
summary(fit.cheddar)

# fecundity of limpets
## Create data (skip if not interested) ####
season <- factor(rep(c("Spring", "Summer"), each = 6))
density <- factor(rep(c(6, 12, 24), each = 3))
y <- c(1.17, 0.50, 1.67, 1.50, 0.83, 1.00, 0.67, 0.67, 0.75,
       4.00, 3.83, 3.83, 3.33, 2.58, 2.75, 2.54, 1.83, 1.63)
design <- expand.grid(density = factor(c(6, 12, 24)), season = c("Spring", "Summer"))
snails <- data.frame(design[rep(1:nrow(design), each = 3), ], y = y)
## Have a look at interaction plot ####
with(snails, interaction.plot(x.factor = density, trace.factor = season, response = y))

fit.limpets <- aov(y ~ season * density, data = snails)
summary(fit.limpets)

snails[, "combined"] <- interaction(snails[, "season"], snails[, "density"])
levels(snails[, "combined"])

fit.comb <- aov(y ~ combined, data = snails)
summary(fit.comb)

library(multcomp)
## 12 vs. 6 in summer
fit.glht <- glht(fit.comb, linfct = mcp(combined = c(0, -1, 0, 1, 0, 0)))
confint(fit.glht)

plot(fit.limpets, which = 2)
qqnorm(rnorm(nrow(snails))) # check vs simulated data

plot(fit.limpets, which = 1, add.smooth = FALSE)
