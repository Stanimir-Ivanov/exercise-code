library(MASS)
data() #list of datasets
help("survey")
data("survey") #makes the survey dataset available
View(survey)
summary(survey)
mean(survey$Pulse, na.rm=T) # the missing values are removed before calculating the mean
boxplot(split(survey$Height, survey$Sex)) # boxplots of two variables (split seperates into two vectors)
plot(survey$Wr.Hnd,survey$NW.Hnd) #scatter plot
mosaicplot(~Sex+Smoke,data=survey) #mosaic plot
plot(survey$Sex,survey$Height)
plot(survey[1:50,2],survey[1:50,3])
boxplot(survey$Height[survey$Sex=="Female"],survey$Height[survey$Sex=="Male"])
# boxplots for males and females separately
View(survey[rev(order(survey$Age))[1:2],"Smoke"])
# order returns a vector of the position of each element of the argument
# rev returns reverses teh order of the argument
boxplot(survey$Wr.Hnd,survey$NW.Hnd) #box plot

Agejung <- survey$Age[survey$Age<30 & !is.na(survey$Pulse)]
Pulsejung <- survey$Pulse[survey$Age<30 & !is.na(survey$Pulse)]
cor(Agejung,Pulsejung)
scatter.smooth(Agejung,Pulsejung,col='red',cex=0.5)
lmobj <- lm(Pulsejung ~ Agejung); plot(Agejung,Pulsejung); abline(lmobj)

