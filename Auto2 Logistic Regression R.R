# I will develop a model to predict whether a given car is classified as high or low gas mileage based on
#the auto2.csv data set. The data in auto2.csv is that of auto.csv with an additional binary variable,
#mpg01, that contains a 1 if mpg contains a value above its median, and a 0 if mpg contains a value below its median.
#(a) Explore the data graphically in order to investigate the association between mpg01 and the other features. Which of the other features seem most likely to be useful in predicting mpg01? Scatterplots and boxplots may be useful tools to answer this question. Describe your findings.
#(b) Split the data into a training set and a test set.
#(c) Perform logistic regression on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (a). What is the test error of the model obtained?
# (d) Vary the threshold level for the probability that assigns the 1 category. How does the test error change?
# (e) Plot a curve that shows the variation of the test error as a function of the threshold level.

Auto2 <- read.csv("~/Downloads/Auto2.csv")
View(Auto2)

autologistic=na.omit(Auto2)
attach(autologistic)
pairs(autologistic)

# From the column "mpg01" in the scatterplot matrix you can already see that acceleration, weight, horsepower, and cylinders
# seem useful predictors. In the regression analysis later we can include all of the variables and then select according to p-value
mpg01=as.factor(mpg01)
plot(mpg01,horsepower)
plot(mpg01,weight)
plot(mpg01,displacement)
plot(mpg01,acceleration)
plot(mpg01,cylinders)
auto0=subset(auto,mpg01=="0")
auto1=subset(auto,mpg01=="1")
plot(auto0$horsepower,auto0$weight,col="blue")
points(auto1$horsepower,auto1$weight,col="red")
# The two-colour scatterplot shows a clear separation between high and low mpg.
# Use logistic regression to estimate the boundary between the two categories.
cylinders=as.factor(cylinders)
auto.reg=glm(mpg01 ~ cylinders+displacement+horsepower+weight+acceleration,family=binomial)
summary(auto.reg)

auto.reg=glm(mpg01 ~ cylinders+displacement,family=binomial)
summary(auto.reg)
# Running glm with all relevant variables first and removing those with highest p-value step by step
# shows that in the end only two predictors are relevant: cylinders and displacement. This is also
# confirmed by an analysis of the correlation matrix, which shows strong correlations
# among the quantitative predictors and thus only one of them should be included in the model
cor(autologistic[,-c(1,2,7,8,9,10)])


# Note that if you run glm with only one predictor such as weight, the p-value will indicate strong statistical significance.
# However, the statistical signficiance disappears when performing logistic regression with all predictors. This is again a c
# confounding effect: weight is strongly correlated with displacement and thus shows strong association when used alone. 

testindex=sample(1:392,150)
auto.train=auto[-testindex,]
auto.test=auto[testindex,]
auto.lreg=glm(mpg01 ~ cylinders+displacement,family=binomial,data=auto.train)
testprob=predict(auto.lreg,auto.test,type="response")
testpred=rep(0,150)
testpred[testprob>0.6]=1
testval=rep(0,150)
testval[auto.test$mpg01==1]=1
table(testpred,testval)

# When you go away from the Bayes optimal value 1/2 the test error
# increases both for larger and smaller thresholds (although depending on your test sample there will be fluctuations.
# Plot a curve using the following code:

n=20 # specifies the number of threshold values
totalerror=rep(1,n+1)   # Initialize values for total error
slist=rep(0,n+1) # Also generate a list of s-values for the plot
for(i in 1:n)
{
  s=i/(n+1) # sets the value of the threshold s, which varies between 0 and 1 as i goes through 1 to n.
  # Using n+1 instead of n prevents the exact value s=1. In this case the confusion matrix has
  # only one row and the code below dows not work. However, for larger n the same will happen due to the
  # the limited size of the dataset.
  slist[i]=s
  testpred=rep(0,150)
  testpred[testprob>s]=1
  # for every value s the confusion matrix is calculated and the false and true positive values recorded
  confusionmatrix=table(testpred,testval)
  totalerror[i]=(confusionmatrix[1,2]+confusionmatrix[2,1])/150
}
plot(slist,totalerror)
