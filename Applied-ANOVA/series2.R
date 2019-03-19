# Q1
d.len <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/lentil.dat", header = TRUE,
                    colClasses = c("NULL", "factor", "integer"))
stripchart(Y ~ TR, vertical = TRUE, data = d.len)

d.len.fit <- aov(Y ~ TR, data = d.len)
summary(d.len.fit)
coef(d.len.fit)
plot(d.len.fit, which = 1)
plot(d.len.fit, which = 2)

# generate a matrix of contrasts
mat.contr <- rbind(c(-6, +1, +1, +1, +1, +1, +1),
c(+0, -1, -1, -1, +1, +1, +1),
c(+0, +2, -1, -1, +2, -1, -1),
c(+0, +0, -1 ,+1, +0, -1, +1),
c(+0, -2, +1, +1, +2, -1, -1),
c(+0, +0, +1, -1, +0, -1, +1))

mat.contr%*%t(mat.contr) # contrasts are orhtogonal because off-diagonal elements are 0

library(multcomp)
fit.mc <- glht(d.len.fit, linfct = mcp(TR = mat.contr))
summary(fit.mc, test = adjusted("none"))

# Q2
el.circuits <- read.csv(file = "elcircuits.csv", header = TRUE, stringsAsFactors = TRUE)
stripchart(ms ~ Type, data = el.circuits, vertical = TRUE)

el.cir.fit <- aov(ms ~ Type, data = el.circuits)
summary(el.cir.fit) 
coef(el.cir.fit)

TukeyHSD(el.cir.fit, which = "Type", conf.level = .95)

mat.el.contr <- rbind(
  c(-1, 2, -1),
  c(1, 0, -1)
)
mat.el.contr %*% t(mat.el.contr)
fit.el.mc <- glht(el.cir.fit, linfct = mcp(Type = mat.el.contr))
summary(fit.el.mc, test = adjusted("bonferroni"))

# Q3
cars <- read.table(file = "http://stat.ethz.ch/Teaching/Datasets/WBL/automob.dat",
                   header = TRUE)
plot(x = cars$AUTO, y = cars$KMP4L, col = as.numeric(cars$STADT))
cars$AUTO <- as.factor(cars$AUTO)
cars$STADT <- as.factor(cars$STADT)
cars.fit <- aov(KMP4L ~ AUTO * STADT, data = cars)
summary(cars.fit)
plot(cars.fit, which = 1)
plot(cars.fit, which = 2)
with(cars, interaction.plot(x.factor = STADT, trace.factor = AUTO, response = KMP4L))
cities <- levels(cars$STADT)
fit.cars.la <- aov(KMP4L ~ AUTO, data = subset(cars, STADT == cities[1]))
fit.cars.pt <- aov(KMP4L ~ AUTO, data = subset(cars, STADT == cities[2]))
fit.cars.sf <- aov(KMP4L ~ AUTO, data = subset(cars, STADT == cities[3]))
summary(fit.cars.la)
summary(fit.cars.pt)
summary(fit.cars.sf)
fit.cars.no.sf <- aov(KMP4L ~ AUTO * STADT, data = subset(cars, STADT != cities[3]))
summary(fit.cars.no.sf)
# Q4
platelets <- read.table(file = "http://stat.ethz.ch/Teaching/Datasets/WBL/smoking.dat",
                        header = TRUE)
platelets$PERIODE <- as.factor(platelets$PERIODE)
platelets$PERSON <- as.factor(platelets$PERSON)
with(platelets, 
     interaction.plot(x.factor = PERIODE, trace.factor = PERSON, response = AGGREG))
fit.platelets <- aov(AGGREG ~ PERSON + PERIODE, data = platelets)
summary(fit.platelets)
t.test(AGGREG ~ PERIODE, data = platelets)
# Q5
# a
x = seq(from = 0, to = 5, length.out = 1000)
plot(x, df(x, df1 = 3, df2 = 1), xlim = c(0, 5), ylim = c(0, .8), type = "l")
curve(df(x, df1 = 3, df2 = 5), type = "l", add = TRUE, col = 'red')
curve(df(x, df1 = 3, df2 = 10), type = "l", add = TRUE, col = 'green')
curve(df(x, df1 = 3, df2 = 20), type = "l", add = TRUE, col = 'blue')
#b
qf(0.95, df1 = 3, df2 = 1)
qf(0.95, df1 = 3, df2 = 5)
qf(0.95, df1 = 3, df2 = 10)
qf(0.95, df1 = 3, df2 = 20)
# c
plot(x, df(x, df1 = 1, df2 = 20), xlim = c(0, 5), ylim = c(0, .8), type = "l")
curve(df(x, df1 = 5, df2 = 20), type = "l", add = TRUE, col = 'red')
curve(df(x, df1 = 10, df2 = 20), type = "l", add = TRUE, col = 'green')
curve(df(x, df1 = 20, df2 = 20), type = "l", add = TRUE, col = 'blue')
# d
qf(0.95, df1 = 20, df2 = 1)
qf(0.95, df1 = 20, df2 = 5)
qf(0.95, df1 = 20, df2 = 10)
qf(0.95, df1 = 20, df2 = 20)
#e
pf(q = 2.37, df1 = 20, df2 = 1, lower.tail = FALSE)
pf(q = 2.37, df1 = 20, df2 = 5, lower.tail = FALSE)
pf(q = 2.37, df1 = 20, df2 = 10, lower.tail = FALSE)
pf(q = 2.37, df1 = 20, df2 = 20, lower.tail = FALSE)
  
