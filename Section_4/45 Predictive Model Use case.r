library(MASS)
library(car)



#***************************************
#***** Simple Linear Regression ********
#***************************************


#We will be using Boston Dataset: 

head(Boston)
fix(Boston)

?Boston
# Run the linear regression: 
lm.fit=lm(medv~lstat,data=Boston)

#Alternate to specifying the data: 
attach(Boston)
lm.fit=lm(medv~lstat)


# See the summary: 
summary(lm.fit)

#See the elements of the model object: 
names(lm.fit)

#Extract the coefficients: 
coef(lm.fit)

#Get the confidence intervals for coefficients: 
confint(lm.fit)

#Use the predict function: 
predict(lm.fit,data.frame(lstat=(c(5,10,15))))

# Also get the confidence intervals for the prediction: 

predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")


#Plot the x and the y variable 
plot(lstat,medv)

# Add the regression line 
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")




#***************************************
#***** Multiple Linear Regression ******
#***************************************

# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)

# Regress all other variables on y 
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

# Use all variables except one ( age) 
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age)

# VIF 

?vif # see help
vif(lm.fit)


# Introduction of the Interaction Terms: 
summary(lm(medv~lstat*age,data=Boston))
