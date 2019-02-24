## Introduction

Before, we move into Regression, here is a brief about the tests one should do depending upon the type of dependent and independent 
variable.   
1. Independent is Nominal and Dependent is Nominal: T-Test  
2. Independent is Nominal and Dependent is Interval: ANOVA  
3. Independent is Interval and Dependent is Interval: Linear Regression  
4. Independent is Interval and Dependent is Nominal: Logistic Regression  

If you have multiple predictors:
1. At least one independent variable is interval and dependent is Interval: Multiple Regression  
2. At least one independent variable is interval and dependent is Nominal: Logistic Regression

Linear Regression is a statistical approach which models a linear relationship between the independent and the dependent variable. 

**Process Equation:**  
Y = β0 + β1X1 + β2X2 + …. + βkXk + Ɛ (variability in the process)

**Fitted Model to the sample:**  
Ŷ = β̂0 + β̂1X1 + β̂2X2 + ……+ β̂kXk
All the above are the estimates to the corresponding variables

## Finding the best line:  
There can be many straight lines passing through the sample points. So, how to come up with our best line?
a.)	Calculate the deviation of Y estimate and Y actual for every data point
b.)	Calculate the sum of squares of these deviations
c.)	Minimize this error
There will be only one line which satisfies the above criteria

## Inference:

**Model Inference:**  
*Null Hypothesis:* There is no relationship with any of the independent variable to the dependent variable i.e., β0 = β1 = β2 =…. βk = 0  
*Alternative Hypothesis:* There is at least one independent variable which has relation with the dependent variable i.e., βi ≠ 0  
*Statistical Test:* Anova Test using F Statistic  

**Individual Predictor Inference:**  
*Null Hypothesis:* There is no relationship with this independent variable to the dependent variable i.e., βi = 0  
*Alternative Hypothesis:* There is a relationship with this independent variable to the dependent variable i.e., βi ≠ 0  
*Statistical Test:* T-Test  

## Assumptions:
1.	Random Sampling: Our sampling data is unbiased 
2.	Stability: The process is stable during the data collection and the analysis period
3.	E is normally distributed with mean 0 and SD Sigma
4.	A linear relationship between the Independent variables and dependent variables
5.	No relationship (correlation) among the independent variables

**How to check these assumptions:**  
1.	For error, to check normal distribution – we can use histogram
2.	For mean 0 assumption, plot residual plot to see how the residuals are distributed around 0.
3.	For constant SD, we again look at residual plot. If there is trend (for example, a constant increase), the assumption is violated. This violation is called Heteroscedasticity. 
 
