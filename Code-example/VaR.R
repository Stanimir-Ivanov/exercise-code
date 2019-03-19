library(SpatialExtremes)
library(rugarch)
library(extRemes)

##----------------------------------------------------------------------------------------------------------##
## Function to calculate 1-day and h-day ahead q% Value-at-Risk (VaR) on log returns of financial assets.
## 
## The function uses a two step rolling sample GARCH-EVT method. 
##
## In the first step, volatility is filtered out through a GARCH specification, fitted on the last 1000 observations. 
## In the second step, the one day ahead q% VaR is estimated by fitting a Generalized Pareto Distribution (GPD) to the 
## first 100 ordered residuals resulting from the GARCH estimation and calculating its qth quantile. 
##
## The h-day VaR is obtained by taking the quantile of simulated n, h-day return paths. 
## The n paths are simulated by picking a residual at random h times and pluggining into the GARCH specification. 
## If the residual belongs to 10% of the extremes of its empirical distribution, a new residual is instead
## simulated from the fitted GPD.
##----------------------------------------------------------------------------------------------------------##
VaR_estimation <- function(specification, data, q, h, n)
{
  VaR_results <- rollapply(data, width = 1000,
                           FUN = function(data)
                           {
                             ## ONE DAY AHEAD VAR ---
                             # Estimate GARCH model and standirdized residuals
                             garch <- estimate_garch(specification, data)
                             mu <- garch$forecast@forecast$seriesFor
                             sigma <- garch$forecast@forecast$sigmaFor
                             innovations <- garch$fit@fit$z
                             
                             # Estimate GPD distribution coefficients
                             gpd <- estimate_gpd(innovations)
                             gpd_coef <- gpd$right_tail$results$par
                             
                             # Calculate quantile of the innovations
                             beta <- gpd_coef['scale']
                             xi <- gpd_coef['shape']
                             threshold <- quantile(innovations, 0.899);
                             
                             # Calculate day-ahead VaR
                             VaR_1day <- calculate_VaR(threshold, beta, xi, mu, sigma, q)                                                                                      
                             
                             ## H-DAY AHEAD VAR ---
                             # Define coefficients
                             mu_t <- mu
                             sigma_t <- sigma
                             ARmu <- garch$fit@fit$coef['mu']
                             phi <- garch$fit@fit$coef['ar1']
                             omega <- garch$fit@fit$coef['omega']
                             alpha1 <- garch$fit@fit$coef['alpha1']
                             beta1 <- garch$fit@fit$coef['beta1']
                                                          
                             simulations <- as.matrix(1:n)
                             paths <- apply(simulations, MARGIN = 1, PATHS <- function(k)
                             {
                               hdays <- as.matrix(1:h)
                               return (sum(apply(hdays, MARGIN = 1,
                                                 HDAYS <- function(j) 
                                                 {
                                                   z <- rand_innovation(innovations, gpd)
                                                   innovations <- z$innovations
                                                   xt <- mu_t + sigma_t * as.numeric(z$z)
                                                   eps <- xt - mu_t
                                                   mu_t <- phi*xt
                                                   sigma_t <- (omega + alpha1*eps^2 + beta1*sigma_t^2)^(1/2)
                                                   return(xt)
                                                 })))
                             })
                             VaR_hday <- quantile(paths, q)
                             return(c(VaR_1day, t(VaR_hday)))
                           },
                           by.column=FALSE, align="right")
  VaR_results <- lag(VaR_results)
  names <- c(paste("1 Day VaR", q), paste(h, "Day VaR", q))
  colnames(VaR_results) <- names
  VaR_results <- VaR_results[!is.na(VaR_results[,1])]
  return(VaR_results)  
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##


##----------------------------------------------------------------------------------------------------------##
## Function to estimate GARCH model and forecast 1-step ahead.
##----------------------------------------------------------------------------------------------------------##
estimate_garch <- function(specifications, data_1)
{
  #Fit the GARCH model.
  garch_fit <- ugarchfit(spec = specifications, data = data_1, solver = "hybrid")
  
  #Forecast according to the input values.
  garch_forecast <- ugarchforecast(fitORspec = garch_fit, data = data_1, n.ahead = 1)
  
  #Return the specifications and estimation of the GARCH model.
  return(list(fit = garch_fit, forecast = garch_forecast))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##


##----------------------------------------------------------------------------------------------------------##
## Function to estimate GPD distribution.
##----------------------------------------------------------------------------------------------------------##
estimate_gpd <- function(innovations_garch)
{
  right_tail <- fevd(innovations_garch, type = "GP", threshold = quantile(innovations_garch, 0.90))
  left_tail <- fevd(-innovations_garch, type = "GP", threshold = quantile(-innovations_garch, 0.90))
  return(list(left_tail = left_tail, right_tail = right_tail))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------## 


##----------------------------------------------------------------------------------------------------------##
## Function to calculate VaR.
##----------------------------------------------------------------------------------------------------------##
calculate_VaR <- function(threshold, beta, xi, mu, sigma, q)
{
  innovation_quantile = innovation_quantile(threshold, beta, xi, q)
  return(mu + sigma * innovation_quantile)
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------## 


##----------------------------------------------------------------------------------------------------------##
## Function to calculate innovations quantile
##----------------------------------------------------------------------------------------------------------##
innovation_quantile <- function(threshold, beta, xi, q)
{
  return(threshold + 
           (beta/xi)*(((1 - q)*1000/100)^(-xi) - 1));  
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------## 


##----------------------------------------------------------------------------------------------------------##
## Random innovation sumulation function.
##----------------------------------------------------------------------------------------------------------##
rand_innovation <- function(z_vec, gpd)
{  
  r_threshold <- as.numeric(quantile(z_vec,0.9))
  l_threshold <- as.numeric(quantile(z_vec,0.1))
  
  r <- sample(1:1000, 1)
  if (z_vec[r] > r_threshold)
  {
    z_vec[r] <- r_threshold + rgpd(1, loc = 0, scale = gpd$right_tail$results$par['scale'],
                                   shape = gpd$right_tail$results$par['shape'])
  }
  if (z_vec[r] < l_threshold)
  {
    z_vec[r] <- l_threshold - rgpd(1, loc = 0, scale = gpd$left_tail$results$par['scale'],
                                   shape = gpd$left_tail$results$par['shape'])
  }
  return(list(innovations = z_vec, z = z_vec[r]))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##