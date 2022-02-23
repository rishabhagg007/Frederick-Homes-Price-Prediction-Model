library(dplyr)
library(tidyverse)
library(car)
library(olsrr)
setwd("C:/Users/dell/Downloads/")
Frederick <- read.csv("Frederick Homes.csv")
View(Frederick)
AskingPrice <- Frederick$Asking.Price
LivSpc <- Frederick$Living.Space
LotSize <- Frederick$Lot.Size
Tax <- Frederick$Yearly.Taxes
Bed<- Frederick$Bedrooms
Bath <- Frederick$Bathrooms
Age <- Frederick$Age
Park <- Frederick$Parking.Spaces
AC <- Frederick$Central.A.C
Base <- Frederick$Finished.Basement
Brick <- Frederick$Brick
Fire <- Frederick$Fireplace


##Checking if multicollinearity exists
cor(Frederick[2:13]) 

##Informal method to check multicollinearity  by plotting
pairs(Frederick[2:13], panel = panel.smooth)

### First run a regression model with all variables for a quick check  for multicollinearity by getting VIF values.
Model_Full <- lm(AskingPrice ~ LivSpc+LotSize+Tax+Bed+Bath+Age+Park+AC+Base+Brick+Fire)
summary(Model_Full)
car::vif(Model_Full)



### Creating a separate data frame with all variables
df_Frederick <- data.frame(AskingPrice,LivSpc,LotSize,Tax,Bed,Bath,Age,Park,AC,Base,Brick,Fire)

### Run Forward Selection
fwd <- ols_step_forward_p(Model_Full, penter = 0.10, details = TRUE)
fwd


### Run Backward Elimination
bwd <- ols_step_backward_p(Model_Full, prem = 0.10, details = TRUE)
bwd

### Run Stepwise Regression
model <- lm(AskingPrice ~ ., data = df_Frederick)
k <- olsrr::ols_step_both_p(model, prem = 0.10, pent = 0.10, details = TRUE)
plot(k)

### Run Best Subsets Regression
best_subsets <- ols_step_best_subset(Model_Full, details = TRUE)
best_subsets

###From the output of Best Subsets Regression , it can be clearly seen that Model 5 has the least Cp, AIC, SBC,MSEP scores , hence Model 5 with explanatory variables LivSpc, Tax ,Age ,Brick ,Fire  is selected



#### Running Without dummy variables
df_Frederick_nodummy <- data.frame(AskingPrice,LivSpc,LotSize,Tax,Bed,Bath,Age,Park)



### Run Stepwise Regression
model_stepwise <- lm(AskingPrice ~ ., data = df_Frederick_nodummy)
k <- olsrr::ols_step_both_p(model_stepwise, prem = 0.10, pent = 0.10, details = TRUE)
plot(k)


### Run Best Subsets Regression
best_subsets <- ols_step_best_subset(model_stepwise, details = TRUE)
best_subsets



### Consider selected variables only and create a separate data frame
df_sel_Frederick<- data.frame(AskingPrice, LivSpc, Tax ,Age, Brick )

### Create a scatterplot to see if there is any evidence of nonlinearity.

pairs(df_sel_Frederick[1:5], panel = panel.smooth)


###From the plot it can be seen that no nonlinearity is been found


Model_Full_a <- lm(AskingPrice ~ LivSpc+ Tax +Age +Brick +Fire )
summary(Model_Full_a)


###Since p-value of coefficient of Fire is insignificant it can be removed from the model as well as it can be seen that by pricible of parismony lesser variables are preferred , as well as stepwise regression model with 4 variables LivSpc, Tax ,Age ,Brick was picked as best and has least BIC scores. Therefore our final model is

###Final model

Model_Final <- lm(AskingPrice ~ LivSpc+ Tax +Age +Brick )
summary(Model_Final)



### Regression diagnostics...
resids <- residuals(Model_Final )
pred <- predict(Model_Final )


###Informal test for checking constant variance i.e hetroscedasticity
plot(pred, resids)


##From the plot it can be clearly seen that variance is constant for residuals, that the variances of the error terms are equal.



# Normality Assessment

#Informal Method for normality
hist(residuals(Model_Final))

## Informal Test: Normal Quantile Plot
qqnorm(residuals(Model_Final))
qqline(residuals(Model_Final))

##From the plot, it appears residuals are normally distributed.



#Formal method to check Normality
# Look at the p-value from Anderson-Darling normality test
# H0: Residuals are normally distributed
# H1: Residuals are not normally distributed
nortest::ad.test(residuals(Model_Final))

####Since p value is less than 0.05, reject H0, hence residuals seems not normally distributed


###Formal method of Independence Assessment
## Durbin-Watson test will help us assess the condition of independence.

# H0: No autocorrelationrelation
# H1: Autcorrelation exists
library(lmtest)
lmtest::dwtest(Model_Final)

###Since p-value is insignificant it can be concluded that no autocorrelation exists.

###Formal test for Hetroscedaticity

### Checking if there is evidence of heteroscedasticity
resids_sq <- (residuals(Model_Final))^2

### Obtain the predicted values and square them
pred_sq <- predict(Model_Final)^2

### Running the auxiliary regression
### Resid_sq = alpha1 + alpha2*PredictedY_sq + vi
model_diagnostic <- lm(resids_sq ~ pred_sq)
summary(model_diagnostic)

###Since p-value is insignificant, it can be concluded that , no heteroscedasticity exists.

##Therefore , all OLS assumtions have been satisfied and overall model is valid.

###The final best possible estimated regression model is 
## AskingPrice = 31.572 -0.008* LivSpc + 0.109*Tax - 0.551*Age + 8.846*Brick 

#R-squared value is 98.76, which means over 98% of the variation is explained by the model.

#Interpretation of coefficients
#LivSpc - If living space increases by 1 unit, Asking price will decrease by 0.008 times, holding all other variables constant
#Tax - If Tax increases by 1 unit, Asking price will increase by 0.109 times, holding all other variables constant
#Age - If Age of houses increases by 1 unit, Asking price will decrease by 0.551 times, holding all other variables constant.
#Brick - If Brick is true for houses, Asking price will increase by 8.846 times, holding all other variables constant

