library(xts)
library(zoo)
source("VaR.R")

q <- c(.95, .99, .995)
h <- 10
n <- 1000

# example garch specification
specification <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 0)), 
                              distribution.model ="norm")

# Import data and convert into time series format
sp <- xts(read.zoo("./SP-500.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1))

# Negative log returns
sp <- - 100*diff(log(sp))

# Run on only the first few weeks to decrease running time
sp_data <- sp[2:1050]

# compute VaR
res_VaR <- VaR_estimation(specification = specification, data = sp_data, q, h, n)

# visualize
plot.xts(res_VaR)
