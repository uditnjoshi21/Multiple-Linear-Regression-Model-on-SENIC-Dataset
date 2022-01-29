############################# MA4710 Project ###################################
# Final Project
# Created by Udit Joshi


############################## Read a data into R ##############################
senic <- read.csv("C:/Users/Uditnjoshi/Desktop/Fall_Sem/MA4710/Assignment/Project/SENIC.csv", header=TRUE)
View(senic)
head(senic)

########################### 1. Introduction  ###################################

###################### 1.1 Exploratory Data Analysis. ##########################
## 1. Histograms of Y and Xs
hist(senic$Y,xlab="Length of Stay (Y)", main="Histogram of Y")

par(mfrow=c(3,4))
hist(senic$X1,xlab="Age (X1)", main="Histogram of X1")
hist(senic$X2,xlab="Infection Risk (X2)", main="Histogram of X2")
hist(senic$X3,xlab="Routine Culturing Ratio (X3)", main="Histogram of X3")
hist(senic$X4,xlab="Routine Chest X-ray Ratio (X4)", main="Histogram of X4")
hist(senic$X5,xlab="Number of Beds (X5)", main="Histogram of X5")
hist(senic$X6,xlab="Medical School (X6)", main="Histogram of X6")
hist(senic$X7,xlab="Region (X7)", main="Histogram of X7")
hist(senic$X8,xlab="Average daily Census (X8)", main="Histogram of X8")
hist(senic$X9,xlab="Number of nurses (X9)", main="Histogram of X9")
hist(senic$X10,xlab="Available facilities and services (X10)", main="Histogram of X10")


## 2. Boxplots of Y and Xs
boxplot(senic$Y, xlab="Y", main="Boxplot of Y")

par(mfrow=c(2,5))
boxplot(senic$X1, xlab="X1", main="Boxplot of X1")
boxplot(senic$X2, xlab="X2", main="Boxplot of X2")
boxplot(senic$X3, xlab="X3", main="Boxplot of X3")
boxplot(senic$X4, xlab="X4", main="Boxplot of X4")
boxplot(senic$X5, xlab="X5", main="Boxplot of X5")
boxplot(senic$X6, xlab="X6", main="Boxplot of X6")
boxplot(senic$X7, xlab="X7", main="Boxplot of X7")
boxplot(senic$X8, xlab="X8", main="Boxplot of X8")
boxplot(senic$X9, xlab="X9", main="Boxplot of X9")
boxplot(senic$X10, xlab="X10", main="Boxplot of X10")


## 3. Summary Statistics 
summary(senic)


## 4. Scatter Plot Matrix 
pairs(senic, col="red", main = "Scatter-Plot matrix of SENIC data") 

par(mfrow=c(3,4))
plot(Y~X1, senic,col="blue", main="Scatter-Plot between Y and X1")
plot(Y~X2, senic,col="blue", main="Scatter-Plot between Y and X2")
plot(Y~X3, senic,col="blue", main="Scatter-Plot between Y and X3")
plot(Y~X4, senic,col="blue", main="Scatter-Plot between Y and X4")
plot(Y~X5, senic,col="blue", main="Scatter-Plot between Y and X5")
plot(Y~X6, senic,col="blue", main="Scatter-Plot between Y and X6")
plot(Y~X7, senic,col="blue", main="Scatter-Plot between Y and X7")
plot(Y~X8, senic,col="blue", main="Scatter-Plot between Y and X8")
plot(Y~X9, senic,col="blue", main="Scatter-Plot between Y and X9")
plot(Y~X10, senic,col="blue", main="Scatter-Plot between Y and X10")


## 5. Added-Variable Plots
library(car)
senic.lmfit <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data = senic)
avPlots(senic.lmfit)


## 6. correlation matrix
cor(senic)

install.packages("corrplot")
library(corrplot)
corrplot(cor(senic))



########################### 2. Model/Methods ###################################

## Fit a regression model with all of the predictors
full.lmfit <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data = senic)
summary(full.lmfit)

## Model Selection (Stepwise Regression)
# Install packages for the model selection 
install.packages("leaps")
install.packages("HH")
install.packages("StepReg")

# Load HH, leaps, and StepReg packages
library(leaps)
library(HH)
library(StepReg)

par(mfrow=c(1,1))
#### Stepwise Regression

#### Adjusted R2
b = bestsubset(data=senic,y="Y",select="adjRsq",best=5)
print(b)
stepwise(data=senic,y="Y",select="adjRsq")
plot(b[,1:2])

#### Cp
b = bestsubset(data=senic,y="Y",select="CP",best=5)
print(b)
stepwise(data=senic,y="Y",select="CP")
plot(b[,1:2])

#### AIC
b = bestsubset(data=senic,y="Y",select="AIC",best=5)
print(b)
stepwise(data=senic,y="Y",select="AIC")
plot(b[,1:2])

#### BIC
b = bestsubset(data=senic,y="Y",select="BIC",best=5)
print(b)
stepwise(data=senic,y="Y",select="BIC")
plot(b[,1:2])

# Now try with the interactive term and include only those variable which is selected from Stepwise Regression.
#i.e., "X2", "X7", "X8", "X9", "X1", "X4", "X5"

# Model with interactive terms
X2X7 <- senic$X2*senic$X7 ;
X2X8 <- senic$X2*senic$X8 ; X7X8 <- senic$X7*senic$X8 ; 
X2X9 <- senic$X2*senic$X9 ; X7X9 <- senic$X7*senic$X9 ; X8X9 <-senic$X8*senic$X9 ; 
X2X1 <- senic$X2*senic$X1 ; X7X1 <- senic$X7*senic$X1 ; X8X1 <-senic$X8*senic$X1 ; X9X1 <-senic$X9*senic$X1 ; 
X2X4 <- senic$X2*senic$X4 ; X7X4 <- senic$X7*senic$X4 ; X8X4 <-senic$X8*senic$X4 ; X9X4 <-senic$X9*senic$X4 ; X1X4 <-senic$X1*senic$X4 ;
X2X5 <- senic$X2*senic$X5 ; X7X5 <- senic$X7*senic$X5 ; X8X5 <-senic$X8*senic$X5 ; X9X5 <-senic$X9*senic$X5 ; X1X5 <-senic$X1*senic$X5 ; X4X5 <-senic$X4*senic$X5 ;

senic.itact <-  cbind(senic,  
                      X2X7 , 
                      X2X8 , X7X8 , 
                      X2X9 , X7X9 , X8X9 , 
                      X2X1 , X7X1 , X8X1 , X9X1 , 
                      X2X4 , X7X4 , X8X4 , X9X4 , X1X4 ,
                      X2X5 , X7X5 , X8X5 , X9X5 , X1X5 , X4X5) 
head(senic.itact)
cor(senic.itact)  
corrplot(cor(senic.itact))             #correlation matrix with interactive term

# As we can see, lots of variables are highly correlated.
#Therefore Standardization needed for our variables.
x1 <- (senic$X1 -mean(senic$X1))/sd(senic$X1)
x2 <- (senic$X2 -mean(senic$X2))/sd(senic$X2)
x3 <- (senic$X3 -mean(senic$X3))/sd(senic$X3)
x4 <- (senic$X4 -mean(senic$X4))/sd(senic$X4)
x5 <- (senic$X5 -mean(senic$X5))/sd(senic$X5)
x6 <- (senic$X6 -mean(senic$X6))/sd(senic$X6)
x7 <- (senic$X7 -mean(senic$X7))/sd(senic$X7)
x8 <- (senic$X8 -mean(senic$X8))/sd(senic$X8)
x9 <- (senic$X9 -mean(senic$X9))/sd(senic$X9)
x10 <- (senic$X10 -mean(senic$X10))/sd(senic$X10)

x2x7 <- x2*x7 ;
x2x8 <- x2*x8 ; x7x8 <- x7*x8 ; 
x2x9 <- x2*x9 ; x7x9 <- x7*x9 ; x8x9 <-x8*x9 ; 
x2x1 <- x2*x1 ; x7x1 <- x7*x1 ; x8x1 <-x8*x1 ; x9x1 <-x9*x1 ; 
x2x4 <- x2*x4 ; x7x4 <- x7*x4 ; x8x4 <-x8*x4 ; x9x4 <-x9*x4 ; x1x4 <-x1*x4 ;
x2x5 <- x2*x5 ; x7x5 <- x7*x5 ; x8x5 <-x8*x5 ; x9x5 <-x9*x5 ; x1x5 <-x1*x5 ; x4x5 <-x4*x5 ;

senic.itact.Std <- cbind(senic$Y,x1,x2,x4,x5,x7,x8,x9,
                         x2x7 , 
                         x2x8 , x7x8 , 
                         x2x9 , x7x9 , x8x9 , 
                         x2x1 , x7x1 , x8x1 , x9x1 , 
                         x2x4 , x7x4 , x8x4 , x9x4 , x1x4 ,
                         x2x5 , x7x5 , x8x5 , x9x5 , x1x5 , x4x5)
colnames(senic.itact.Std)[1] <- "Y"
head(senic.itact.Std)
cor(senic.itact.Std)  
corrplot(cor(senic.itact.Std))         #correlation matrix after Standardization 
#Now very few of them are highly correlated to each other compare to without standardization.

senic.itact.Std <- as.data.frame(senic.itact.Std)    # Converting to data Frame.
head(senic.itact.Std)


################ Fit a regression model with interaction terms
full.lmfit <- lm(Y ~ x1+x2+x4+x5+x7+x8+x9+     
                   x2x7 + 
                   x2x8 + x7x8 + 
                   x2x9 + x7x9 + x8x9 + 
                   x2x1 + x7x1 + x8x1 + x9x1 + 
                   x2x4 + x7x4 + x8x4 + x9x4 + x1x4 +
                   x2x5 + x7x5 + x8x5 + x9x5 + x1x5 + x4x5, data = senic.itact.Std)
summary(full.lmfit)

################ Fit a regression model with no interactive terms
reduced.lmfit <- lm(Y ~ x1+x2+x4+x5+x7+x8+x9, data=senic.itact.Std)
summary(reduced.lmfit)

################ Test for significance of the interactive terms
anova(reduced.lmfit, full.lmfit)


## Again Model Selection (Stepwise Regression) after including interactive terms.
#### Stepwise Regression
#### Adjusted R2
stepwise(data=senic.itact.Std,y="Y",select="adjRsq")

#### Cp
stepwise(data=senic.itact.Std,y="Y",select="CP")

#### AIC
stepwise(data=senic.itact.Std,y="Y",select="AIC")

#### BIC
stepwise(data=senic.itact.Std,y="Y",select="BIC")


################ Fit a regression model with interaction terms
full.lmfit <- lm(Y ~ x2 + x7 + x2x8 + x2x5 + x8 + x9 + x1 + x4 + x2x4 + x8x5, data = senic.itact.Std)
summary(full.lmfit)
anova(full.lmfit)

#In the summary table we have seen one interactive variable is have higher p-value than 0.1 so we will drop this column
full.lmfit <- lm(Y ~ x2 + x7 + x2x8 + x2x5 + x8 + x9 + x1 + x4 + x2x4, data = senic.itact.Std)
summary(full.lmfit)

################ Fit a regression model with no interaction terms
reduced.lmfit <- lm(Y ~ x2 + x7 + x8 + x9 + x1 + x4, data=senic.itact.Std)
summary(reduced.lmfit)

################ Test for significance of the interaction terms
anova(reduced.lmfit, full.lmfit)

# Based on the anova test, we will go with the full model as our final model.
#### Final Model
reduced.lmfit <- lm(Y ~ x2 + x7 + x2x8 + x2x5 + x8 + x9 + x1 + x4 + x2x4, data=senic.itact.Std)
summary(reduced.lmfit)



######################## 3. Assumption Checking ################################
##### 3.1 Linearity assumption checking. #####
data.fitted <- fitted(reduced.lmfit)
data.res <- rstudent(reduced.lmfit)                  #jackknifed residual

# Residual plot against fitted values. 
plot(data.res ~ data.fitted, xlab="fitted values", ylab="residuals", main="residual plot against fitted values")

## Residual plot against predictor variables.
par(mfrow=c(3,3))
plot(data.res ~ senic.itact.Std$x2, xlab="x2", ylab="residuals", main="residual plot against x2")
plot(data.res ~ senic.itact.Std$x7, xlab="x7", ylab="residuals", main="residual plot against x7")
plot(data.res ~ senic.itact.Std$x2x8, xlab="x2x8", ylab="residuals", main="residual plot against x2x8")
plot(data.res ~ senic.itact.Std$x2x5, xlab="x2x5", ylab="residuals", main="residual plot against x2x5")
plot(data.res ~ senic.itact.Std$x8, xlab="x8", ylab="residuals", main="residual plot against x8")
plot(data.res ~ senic.itact.Std$x9, xlab="x9", ylab="residuals", main="residual plot against x9")
plot(data.res ~ senic.itact.Std$x1, xlab="x1", ylab="residuals", main="residual plot against x1")
plot(data.res ~ senic.itact.Std$x4, xlab="x4", ylab="residuals", main="residual plot against x4")
plot(data.res ~ senic.itact.Std$x2x4, xlab="x2x4", ylab="residuals", main="residual plot against x2x4")


##### 3.2 Constant Variance assumption checking.  ##### 
## Residual plot against fitted values. 
plot(data.res ~ data.fitted, xlab="fitted values", ylab="residuals", main="residual plot against fitted values")
abline(h=0,lwd=2)

## Residual plot against predictor variables.
par(mfrow=c(3,3))
plot(data.res ~ senic.itact.Std$x2, xlab="x2", ylab="residuals", main="residual plot against x2")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x7, xlab="x7", ylab="residuals", main="residual plot against x7")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x2x8, xlab="x2x8", ylab="residuals", main="residual plot against x2x8")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x2x5, xlab="x2x5", ylab="residuals", main="residual plot against x2x5")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x8, xlab="x8", ylab="residuals", main="residual plot against x8")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x9, xlab="x9", ylab="residuals", main="residual plot against x9")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x1, xlab="x1", ylab="residuals", main="residual plot against x1")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x4, xlab="x4", ylab="residuals", main="residual plot against x4")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x2x4, xlab="x2x4", ylab="residuals", main="residual plot against x2x4")
abline(h=0,lwd=2)

## ABS Residual plot against predictor variables. 
par(mfrow=c(3,3))
plot(abs(data.res) ~ senic.itact.Std$x2, xlab="x2", ylab="|residuals|", main="ABS residual plot against x2")
plot(abs(data.res) ~ senic.itact.Std$x7, xlab="x7", ylab="|residuals|", main="ABS residual plot against x7")
plot(abs(data.res) ~ senic.itact.Std$x2x8, xlab="x2x8", ylab="|residuals|", main="ABS residual plot against x2x8")
plot(abs(data.res) ~ senic.itact.Std$x2x5, xlab="x2x5", ylab="|residuals|", main="ABS residual plot against x2x5")
plot(abs(data.res) ~ senic.itact.Std$x8, xlab="x8", ylab="|residuals|", main="ABS residual plot against x8")
plot(abs(data.res) ~ senic.itact.Std$x9, xlab="x9", ylab="|residuals|", main="ABS residual plot against x9")
plot(abs(data.res) ~ senic.itact.Std$x1, xlab="x1", ylab="|residuals|", main="ABS residual plot against x1")
plot(abs(data.res) ~ senic.itact.Std$x4, xlab="x4", ylab="|residuals|", main="ABS residual plot against x4")
plot(abs(data.res) ~ senic.itact.Std$x2x4, xlab="x2x4", ylab="|residuals|", main="ABS residual plot against x2x4")

##### BP test.
library(lmtest)
bptest(reduced.lmfit)


##### 3.3 Independence assumption checking. #####
## Residual plot against time
## Data is not time series related. So, we can't check it.


##### 3.4 Normality assumption checking. #####
## Boxplot of residuals
boxplot(data.res, xlab="residual", main="Boxplot of residuals")

hist(data.res, main="Histogram of residuals", xlab="Residuals",probability=TRUE)
lines(seq(-3,3,length.out = 1000),dnorm(seq(-3,3,length.out = 1000)))

## QQ Plot
qqnorm(data.res); qqline(data.res)


##### Shapiro-Wilk test
shapiro.test(data.res)


##### 3.5 Outliers checking. #####
## Residual plot against fitted values 
plot(data.res ~ data.fitted, xlab="fitted values", ylab="residuals", main="residual plot against fitted values")
abline(h=0,lwd=2)

which(abs(data.res) >3)

## Residual plot against predictor variables
par(mfrow=c(3,3))
plot(data.res ~ senic.itact.Std$x2, xlab="x2", ylab="residuals", main="residual plot against x2")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x7, xlab="x7", ylab="residuals", main="residual plot against x7")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x2x8, xlab="x2x8", ylab="residuals", main="residual plot against x2x8")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x2x5, xlab="x2x5", ylab="residuals", main="residual plot against x2x5")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x8, xlab="x8", ylab="residuals", main="residual plot against x8")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x9, xlab="x9", ylab="residuals", main="residual plot against x9")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x1, xlab="x1", ylab="residuals", main="residual plot against x1")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x4, xlab="x4", ylab="residuals", main="residual plot against x4")
abline(h=0,lwd=2)
plot(data.res ~ senic.itact.Std$x2x4, xlab="x2x4", ylab="residuals", main="residual plot against x2x4")
abline(h=0,lwd=2)


## Boxplot of residuals
boxplot(data.res, xlab="residual", main="Boxplot of residuals")

library(car)
leveragePlots(reduced.lmfit)


############ Detection methods of influential points 
install.packages("olsrr")
library(olsrr)

## 1. DFFITS
ols_plot_dffits(reduced.lmfit)

## 2. Cook's D
ols_plot_cooksd_chart(reduced.lmfit)

## 3. DFBETAS
ols_plot_dfbetas(reduced.lmfit)


##### 3.6 Diagnose Multicollinearity. #####
summary(reduced.lmfit)   
library(dplyr)
install.packages("corrplot")
library("corrplot")
corrplot(cor(select(senic.itact.Std,Y, x2 , x7 , x2x8 ,x2x5, x8 , x9 , x1 , x4, x2x4))) 
anova(reduced.lmfit)

# Check Variance Inflation Factor(VIF).
library(car,carData)
vif(reduced.lmfit)
# Based on VIF we can say that interactive variables x2x8 and x2x5 are having 
# high correlation so remove one of them from the model.

reduced.lmfit <- lm(Y ~ x2 + x7 + x2x8 + x8 + x9 + x1 + x4+ x2x4 , data=senic.itact.Std)
vif(reduced.lmfit)
summary(reduced.lmfit)



######################### 4. Remedial Measures #################################
##### 4.1 Multicollinearity remedial measure. #####
# Based on VIF we can say that interactive variables x2x8 and x2x5 are having 
# highly correlated to each other so remove one of them from the model.
reduced.lmfit <- lm(Y ~ x2 + x7 + x2x8 + x8 + x9 + x1 + x4+ x2x4 , data=senic.itact.Std)
vif(reduced.lmfit)
summary(reduced.lmfit)


##### 4.2 Delete Outliers. #####
model <- lm(Y ~ x2 + x7 + x2x8 + x8 + x9 + x1 + x4+ x2x4, data=senic.itact.Std[-c( 47,43),])
which(abs(data.res) >3)


## Check the Model Overall Significance
summary(model)
model <- lm(Y ~ x2 + x7 + x2x8 + x8 + x9 + x1 + x4, data=senic.itact.Std[-c( 47,43),])
summary(model)
anova(model)



########### 5. Model Assumptions Checking after Remedial Measures. #############
senic.itact.Std_ot <- senic.itact.Std[-c( 47,43),]
model <- lm(Y ~ x2 + x7 + x2x8 + x8 + x9 + x1 + x4, data=senic.itact.Std_ot)
summary(model)


##### 5.1 Linearity assumption checking. #####
data.fitted <- fitted(model)
data.res <- rstudent(model)                  #jackknifed residual

# Residual plot against fitted values. 
plot(data.res ~ data.fitted, xlab="fitted values", ylab="residuals", main="residual plot against fitted values")

## Residual plot against predictor variables.
par(mfrow=c(3,3))
plot(data.res ~ senic.itact.Std_ot$x2, xlab="x2", ylab="residuals", main="residual plot against x2")
plot(data.res ~ senic.itact.Std_ot$x7, xlab="x7", ylab="residuals", main="residual plot against x7")
plot(data.res ~ senic.itact.Std_ot$x2x8, xlab="x2x8", ylab="residuals", main="residual plot against x2x8")
plot(data.res ~ senic.itact.Std_ot$x2x5, xlab="x2x5", ylab="residuals", main="residual plot against x2x5")
plot(data.res ~ senic.itact.Std_ot$x8, xlab="x8", ylab="residuals", main="residual plot against x8")
plot(data.res ~ senic.itact.Std_ot$x9, xlab="x9", ylab="residuals", main="residual plot against x9")
plot(data.res ~ senic.itact.Std_ot$x1, xlab="x1", ylab="residuals", main="residual plot against x1")
plot(data.res ~ senic.itact.Std_ot$x4, xlab="x4", ylab="residuals", main="residual plot against x4")
plot(data.res ~ senic.itact.Std_ot$x2x4, xlab="x2x4", ylab="residuals", main="residual plot against x2x4")


##### 5.2 Constant Variance assumption checking.  ##### 
## Residual plot against fitted values. 
plot(data.res ~ data.fitted, xlab="fitted values", ylab="residuals", main="residual plot against fitted values")
abline(h=0,lwd=2)


##### BP test.
library(lmtest)
bptest(model)


##### 5.3 Independence assumption checking. #####
## Residual plot against time
## Data is not time series related. So, we can't check it.


##### 5.4 Normality assumption checking. #####
## Boxplot of residuals
boxplot(data.res, xlab="residual", main="Boxplot of residuals")

hist(data.res, main="Histogram of residuals", xlab="Residuals",probability=TRUE)
lines(seq(-3,3,length.out = 1000),dnorm(seq(-3,3,length.out = 1000)))

## QQ Plot
qqnorm(data.res); qqline(data.res)


##### Shapiro-Wilk test
shapiro.test(data.res)


##### 5.5 Outliers checking. #####
## Residual plot against fitted values 
plot(data.res ~ data.fitted, xlab="fitted values", ylab="residuals", main="residual plot against fitted values")
abline(h=0,lwd=2)

which(abs(data.res) >3)

library(car)
leveragePlots(model)


############ Detection methods of influential points 
install.packages("olsrr")
library(olsrr)

## 1. DFFITS
ols_plot_dffits(model)

## 2. Cook's D
ols_plot_cooksd_chart(model)


##### 5.6 Diagnose Multicollinearity. #####
summary(model)   
library(dplyr)
install.packages("corrplot")
library("corrplot")
corrplot(cor(select(senic.itact.Std_ot,Y, x2 , x7 , x2x8 , x8 , x9 , x1 , x4, x2x4))) 
anova(model)

# Check Variance Inflation Factor(VIF).
library(car,carData)
vif(model)



############################## 3. Result #######################################
summary(model)
anova(model)
