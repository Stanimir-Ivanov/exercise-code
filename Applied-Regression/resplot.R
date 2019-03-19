## Enhanced Function for Residual Plots
resplot <- function(obj, plots=1:4)
{
  ## Coded by: Marcel Dettling, September 18, 2015
  ## Argument: obj   - a regression fit, i.e. output of R-Function lm()
  ## Argument: plots - which plots to generate (1=Tukey-Anscombe, 2=Normal, 3=Scale-Location, 4=Leverage)
  
  ## Set number of frames in the plot
  if (length(plots)>=3) par(mfrow=c(2,2))
  if (length(plots)==2) par(mfrow=c(1,2))
  if (length(plots)==1) par(mfrow=c(1,1))
  
  ## Set random seed that plots look always the same
  set.seed(21)
  
  ## Tukey-Anscombe-Plot with Resampling
  if (1 %in% plots)
  {
    plot(fitted(obj), resid(obj), pch=20, xlab="Fitted Values", ylab="Residuals")
    title("Tukey-Anscombe-Plot with Resampling")
    for (i in 1:100) lines(loess.smooth(fitted(obj), sample(resid(obj, replace=TRUE))), col="grey")
    abline(h=0, lty=2)
    points(fitted(obj), resid(obj), pch=20); box()
    lines(loess.smooth(fitted(obj), resid(obj)), col="red")
  }
  
  ## Normal Plot with Resampling
  if (2 %in% plots)
  {
    qq <- qqnorm(rstandard(obj), pch=20, main="Normal Plot with Resampling", ylab="Standardized Residuals")
    for (i in 1:100) lines(sort(qq$x), sort(rnorm(length(qq$y), mean(qq$y), sd(qq$y))), col="grey")
    points(qq$x, qq$y, pch=20); box()
    qqline(rstandard(obj), lty=2)
  }
  
  ## Scale-Location-Plot with Resampling
  if (3 %in% plots)
  {
    plot(fitted(obj), sqrt(abs(rstandard(obj))), pch=20, ylab="sqrt(abs(Standardized Residuals))", xlab="Fitted Values", ylim=c(0, range(sqrt(abs(rstandard(obj))), na.rm=TRUE)[2]), main="Scale-Location with Resampling")
    for (i in 1:100) lines(loess.smooth(fitted(obj), sample(sqrt(abs(rstandard(obj))), replace=TRUE)), col="grey")
    points(fitted(obj), sqrt(abs(rstandard(obj))), pch=20); box()
    lines(loess.smooth(fitted(obj), sqrt(abs(rstandard(obj)))), col="red")
  }
  
  ## Leverage Plot (without Resampling, taken from plot.lm()
  if (4 %in% plots)
  {
    plot(obj, which=5, pch=20, caption="")
    title("Leverage Plot")
  }
}