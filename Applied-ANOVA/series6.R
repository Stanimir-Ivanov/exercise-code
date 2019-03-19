
# 1
library(ibd)
bibd1 <- ibd(v = 4, b = 6, k = 2)
bibd1$design

bibd1$conc.mat

bibd2 <- ibd(v = 7, b = 7, k = 3)
bibd2$design
bibd2$conc.mat

# 2
library(crossdes)
ibd <- read.table('http://stat.ethz.ch/Teaching/Datasets/WBL/ibd.dat',
           header = TRUE)
View(ibd)

tab <- xtabs(~block + treatment, data = ibd)
d <- t(apply(tab, 1, function(x) (1:ncol(tab))[x != 0]))
isGYD(d)

ibd.factors <- c("block")
ibd[ibd.factors] <- lapply(ibd[ibd.factors], factor)
str(ibd)

stripchart(y ~ block, data = ibd, vertical = TRUE)
stripchart(y ~ treatment, data = ibd, vertical = TRUE)

fit.ibd <- aov(y ~ block + treatment, data = ibd)
summary(fit.ibd)
drop1(fit.ibd, test = "F")

par(mfrow = c(1, 2))
plot(fit.ibd, which = 1:2)

TukeyHSD(fit.ibd)
