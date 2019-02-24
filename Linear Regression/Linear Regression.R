# Linear Regression Sample R Code

## Building the model
linefit <-lm(house$AppraisedValue ~ house$HouseSize + house$LotSize + house$Age)

## Summary gives the inference about the each independent variable while ANOVA helps in interpreting the model as a whole
summary(linefit)
anova(linefit)

## Assumptions' validation tests:

h <- hist(linefit.stres) 
x <- linefit.stres 
xfit <- seq(min(x), max(x), length = 50) 
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue") 

qqnorm(linefit.stres, main = "Normal Probability Plot", xlab = "Normal Scores ", ylab = "Standardized Residuals") 
qqline(linefit.stres, col = "red") 

shapiro.test (linefit.stres)

linefit.stres <- rstandard(linefit) 
plot(linefit$fitted.values, linefit.stres, pch = 16, main = "Standardized Re sidual Plot", xlab = "Fitted Sales (100s)", ylab = "Standardized Residuals") 
abline(0,0, lty=2, col="red")