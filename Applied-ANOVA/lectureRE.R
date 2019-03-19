## Create data-set ####
## machine 1 machine 2 machine 3 machine 4
y <- c(142.3, 144.0, 148.6, 146.9, 142.9, 147.4, 133.8, 133.2, ## day 1
       134.9, 146.3, 145.2, 146.3, 125.9, 127.6, 108.9, 107.5, ## day 2
       148.6, 156.5, 148.6, 153.1, 135.5, 138.9, 132.1, 149.7, ## day 3
       152.0, 151.4, 149.7, 152.0, 142.9, 142.3, 141.7, 141.2) ## day 4
trigly <- data.frame(y = y, day = factor(rep(1:4, each = 8)),
                     machine = factor(rep(rep(1:4, each = 2), 1)))
str(trigly)

xtabs(~ day + machine, data = trigly)
with(trigly, interaction.plot(x.factor = day, trace.factor = machine, response = y))
