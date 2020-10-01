# Machine-Learning-Techniques

## Basic Modeling Definitions:

**Parametric:** models ﬁrst assume about a function form of the relation between the dependent and the independent variables. Then fit the model. If our assumption was wrong, it will lead to bad results. \
**Non-Parametric:** models that don’t make any assumptions about function, which allows them to ﬁt a wider range of functions/shapes; but may lead to overﬁtting 
**Supervised:** models that ﬁt independent variables to a known dependent variable. \
**Unsupervised:** models that take in independent variables, but they do not have an associated output to supervise the training. The goal is to understand relationships between the variables or observations. \
**Deterministic:** models that produce a single “prediction” e.g. yes or no, true or false. \
**Stochastic:** models that produce probability distributions over possible events. \

## Linear Regression

Linear Regression is a statistical approach which models a linear relationship between the independent and the dependent variable.

### Process Equation:
Y = β0 + β1X1 + β2X2 + …. + βkXk + Ɛ (variability in the process)

### Fitted Model to the sample:
Ŷ = β̂0 + β̂1X1 + β̂2X2 + ……+ β̂kXk All the above are the estimates to the corresponding variables

### Finding the best line:
There can be many straight lines passing through the sample points. So, how to come up with our best line?
1. Calculate the deviation of Y estimate and Y actual for every data point
2. Calculate the sum of squares of these deviations
3. Minimize this error
The above method of identifying the best fit line by minimizing the errors is called least squared errors. There will be only one line which satisfies the above criteria

### Inference:

**Model Inference:**
*Null Hypothesis:* There is no relationship with any of the independent variable to the dependent variable i.e., β0 = β1 = β2 =…. βk = 0
*Alternative Hypothesis:* There is at least one independent variable which has relation with the dependent variable i.e., βi ≠ 0
*Statistical Test:* Anova Test using F Statistic

**Individual Predictor Inference:**
*Null Hypothesis:* There is no relationship with this independent variable to the dependent variable i.e., βi = 0
*Alternative Hypothesis:* There is a relationship with this independent variable to the dependent variable i.e., βi ≠ 0
*Statistical Test:* T-Test

### Assumptions:
1. Random Sampling: Our sampling data is unbiased
2. Stability: The process is stable during the data collection and the analysis period
3. Ɛ is normally distributed with mean 0 and SD Sigma
4. A linear relationship between the Independent variables and dependent variables
5. No relationship (correlation) among the independent variables

### How to check these assumptions:
1. For error, to check normal distribution – we can use histogram, QQNorm Plot & Shapiro Wilk Normality Test
2. For mean 0 assumption, plot residual plot to see how the residuals are distributed around 0.
3. For constant SD, we again look at residual plot. If there is trend (for example, a constant increase), the assumption is violated. This violation is called Heteroscedasticity.

function in R: LM
function in Python: statmodels.api library (OLS function) OR
sklearn.linear_model LinearRegression

## Regularization
Bias tells us the complexity of the model. High Bias means, the model has made many assumptions to make it simple. However, simple models are prone to underfit the data. As we include more variables, the model becomes more complex and becomes less bias.

However, they are prone to overfit the data as well. To overcome this problem, there are 2 approaches – Make the model simple, or regularization.
Making the mode simple might make the model underfit. Hence regularization is used.

In general, the aim to identify the best fit curve by reducing the loss function. In Regularization, along with the sum of distances between each prediction and its ground truth, we add one more term called regularization term. The reason for doing that is to “punish” the loss function for high values of the coefficients β or other way of interpreting this is putting a limit on your coefficients so that with changes in training dataset, the coefficients won’t change very much and hence less variance.

There are 2 ways of regularization:
1. Ridge: We add λΣβ2  to the loss function. A super important fact we need to notice about ridge regression is that it enforces the β coefficients to be lower, but it does not enforce them to be zero. That is, it will not get rid of irrelevant features but rather minimize their impact on the trained model.

Code in Python: from sklearn.linear_model import Ridge
Code in R: glmnet (alpha = 0)

2. To overcome this limitation, we introduce Lasso Regularization
Instead of β2, we add absolute value of β into this loss function. Hence, Lasso method overcomes the disadvantage of Ridge regression by not only punishing high values of the coefficients β but actually setting them to zero if they are not relevant. Therefore, you might end up with fewer features included in the model than you started with, which is a huge advantage.
Code in Python: from sklearn.linear_model import Lasso
Code in R: glmnet (alpha = 1)

## Logistic Regression

Linear Regression is a statistical approach which models a linear relationship between the independent and the dependent variable. In linear regression, we can apply that model only when dependent variable is continuous/ Interval. 
So, what can be done when we have categorical/nominal variable?

**Logistic Regression is a type of classification algorithm involving a linear discriminant.**
Let’s consider 2 class dependent variable (0 and 1). If we have more classes, we can apply multinomial logistic regression. Unlike linear regression, logistic regression does not try to predict the value of a numeric variable given a set of inputs. Instead, the output is a probability that the given input point belongs to a certain class (one side of linear boundary as 0 and other side of linear boundary as 1). The equation “Ŷ = β̂0 + β̂1X1 + β̂2X2 + ……+ β̂kXk” is the equation of the linear boundary between the 2 classes.

**Why can’t we apply linear regression?**
Ŷ = β̂0 + β̂1X1 + β̂2X2 + ……+ β̂kXk is the equation we get out of linear regression. There is a possibility that the Ŷ can go beyond 1 and below 0 as well i.e., -infinity to +infinity. However, we need some function which can provide output only between 0 and 1.

For simplicity, let’s consider only 2 independent variables which can define our linear decision boundary.
Consider a point (a, b). Based on the linear boundary function, we will get its output. Now depending on the location of (a, b), there are three possibilities to consider-
1. (a, b) lies in the region defined by points of the + class. As a result, output of linear function will be positive, lying somewhere in (0, infinity) or wrt probability (0.5 to 1)
2. (a, b) lies in the region defined by points of the – class. As a result, output of linear function will be positive, lying somewhere in (-infinity, 0) or wrt probability (0 to 0.5)
3. (a, b) lies on decision boundary. This means that the model cannot really say whether (a, b) belongs to the + or – class. As a result, P_+ will be exactly 0.5.

So, we have linear function which can go from -infinity to +infinity. So, how can we map it back to probability ranging from 0 to 1? Odds ratio
Odds Ratio is probability of success upon probability of failure. This ratio has a range of 0 to infinity.
OR(x) = p(x)1-p(x) = 0 to infinity
But, we need something which has range from -infinity to +infinity
Let’s take log of it
Log(OR(x)) = log(p(x)1-p(x)) = -infinity to +infinity (log(0) = infinity and log(infinity) = -infinity)

**So, how to extract the probability from above function?**
Log(OR(x)) = y
p(x)1-p(x) = e^y
P(x) = e^y – e^y * p(x)
e^y = p(x) (1 + e^y)
p(x) = e^y1+e^y

### Assumptions:
1. No correlation between the independent variable
2. Assumes linearity of independent variables and log odds.

Code in R: glm function, family = ‘binomial’
Code in Python: sklearn.linear_model, LogisticRegression

## Decision Tree Classification

Tries to identify the groups which are in the purest form based on all the independent variables available in the dataset. At each node, it calculates Information Gain between the parent and the child node. 
Formula : Entropy (Parent) – Entropy(child nodes)
Entropy = -P1Log(P1) – P2Log(P2)

**Advantages:**
Easy to understand – Easy to implement – Easy to use – Computationally cheap

**Disadvantages:**
Prone to overfit, orthogonal boundaries

Code in Python: from sklearn.tree import DecisionTreeClassifier
Code In R:  rpart

### Hyperparameters:
Criteria: Gini vs Entropy
Max_depth: max branches of the tree
Min_sample_split: # of points to split in min
Min_sample_leaf: # of points min to be part to create the child node

## Decision Tree Regression
For decision tree regression, everything is same as decision tree classification except the calculation of information gain. The information gain is calculated by highest reduction in standard deviation from parent to child node.

SD = P(A)*S.d(A) + P(B) *S.d(B)

IG = SD(parent) – SD(Child)

## kNN
The model tries to identify the class of the new data point based on the “k” nearest neighbors around it. Depending upon the k-value, the classification changes. 
How do we say nearest? – We calculate the distance of the new point to all the training points around it and see the class with highest proportion within the k-points.
Euclidean, Minkowski, chisquare etc are various distances which can be used.

**One important point to remember is to scale the data – min max scaling, z-score**
**We can also provide weights depending on distance or provide uniform weightage for all points.**

### Advantages:
Faster to train – easy to implement – no assumptions

### Disadvantages:
More time to predict – needs lot of memory

Code in Python: from sklearn.neighbors import KNeighborsClassifier
Code in R: library(class) & knn

### Hyperparameters:
Distance
K
weights

## Naïve Bayes

It follows bayes theorem with an assumption of independence among predictors. 
P(c/x) = (P(x/c) * P(c))/P(x)
### Steps involved:
1. Convert the data into frequency table
2. Create Likelihood table by finding the probabilities for each class

### Advantages:
Easy and fast to predict – require less data – if it is categorical, it performs well
### Disadvantages:
Assumes independence between the predictors

Code in Python: from sklearn.naive_bayes
Code in R: library(e1071), naiveBayes

## SVM
It is a supervised ML algorithm which can used for both Regression and classification, but majorly for classification. Just like Logistic Regression, it tries to identify a decision boundary which can classify the dataset. The decision boundary which is used to classify are called hyper planes. The data points closest to this hyper plane are called support vectors. 

**How to find the right hyper plane?**
The distance between the hyperplane and the nearest data point from either set is known as the margin. The goal is to choose a hyperplane with the greatest possible margin between the hyperplane and any point within the training set, giving a greater chance of new data being classified correctly.

**But what happens when there is no clear hyperplane?**
You transform the data points into higher dimensions and try to identify a plane which can help us in classifying the dataset.

### Advantages:
Kernel Trick – can solve any complex problem
Works well with high dimensional data
Regularization happens
Works well with unstructured/semi structured data

### Disadvantages:
Isn’t suited to larger datasets as the training time with SVMs can be high
Less effective on noisier datasets with overlapping classes
Choosing kernel is difficult
Difficult to understand and interpret the model

Code in Python: from sklearn.svm import SVC
Code in R: library(e1071); svm

## Random Forest
Random forest is basically a combination of multiple decision trees to improve accuracy. Every decision tree considers a random set of features and random set of training datasets and builds a fully-grown tree. This process continues based on # of trees as a parameter was set, and at the end, all these trees are clubbed together. This increases diversity in the model and hence less overfitting. 

Code in R: library(randomForest)
Code in Python: from sklearn.ensemble import RandomForestRegressor/ RandomForestClassifier

## Gradient Boosting
Boosting is methodology where subsequent algorithms tries to predict the error from it’s previous algorithm. All these models will be in sequence.

Code in Python: from sklearn.ensemble import GradientBoostingClassifier
Code in R: library(gbm)

## XG Boost
XG boost does the same thing as gradient boosting. However it is more regularized one which prevents these models to overfit and hence perform better than gradient boosting.

Code in Python: from xgboost import XGBClassifier
Code in R: library(xgboost)

## Model Comparison
ROC Curve: TP rate vs FP rate
TP rate or Sensitivity:  TP/ TP+FN
FP rate or (1-specificity) = 1- TN/(TN + FP)
AUC
Precision Recall Curve: Precision vs Recall
f-measure : 2* Prec* Rec/(Prec + Rec)

