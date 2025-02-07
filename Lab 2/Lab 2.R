library("ggplot2")
library("readr")

## read dataset
NY_House_Dataset <- read_csv("RPI Grad Year 2024/Spring/Data Analytics/NY-House-Dataset.csv")

dataset <- NY_House_Dataset

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

#fit 3 linear models with price as the response variable 
#and combinations of PropertySqFt, Beds, and Bath as predictors 
#do any data cleaning to get the best possible models 
#for each model print the model summary stats and plot the most significant variable vs price


## filter data
dataset <- dataset[dataset$PRICE<195000000,]

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

dataset$PROPERTYSQFT[dataset$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]

## column names
names(dataset)

## fit linear model
lmod <- lm(PRICE~PROPERTYSQFT, data = dataset)

lmod <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod)

plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")



##Linear model 1: Beds & Bath vs Price 

ggplot(dataset, aes(x = (BATH) + (BEDS), y = log10(PRICE))) +
  geom_point()

## filter data
dataset <- dataset[dataset$PRICE<195000000,]

#dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

dataset <- dataset[dataset$BEDS <20,]
dataset <- dataset[dataset$BATH <10,]
ggplot(dataset, aes(x = (BATH) + (BEDS), y = log10(PRICE))) +
  geom_point()

## fit linear model
lmod <- lm(log10(PRICE)~BATH+BEDS, data = dataset)

## print model output
summary(lmod)

Call:
  lm(formula = log10(PRICE) ~ BATH + BEDS, data = dataset)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.27536 -0.21216 -0.03656  0.17856  1.84295 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 5.534262   0.015133  365.71   <2e-16 ***
  BATH        0.179532   0.007222   24.86   <2e-16 ***
  BEDS        0.003174   0.004809    0.66    0.509    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3521 on 2069 degrees of freedom
Multiple R-squared:  0.3997,	Adjusted R-squared:  0.3991 
F-statistic: 688.8 on 2 and 2069 DF,  p-value: < 2.2e-16


## scatter plot of 2 variables
plot(log10(PRICE)~BATH, data = dataset)
abline(lmod)
plot(log10(PRICE)~BEDS, data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = BATH+BEDS, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="blue")



##Linear model 2: PropertySqFt & Beds vs Price  

ggplot(dataset, aes(x = log10(PROPERTYSQFT) + (BEDS), y = log10(PRICE))) +
  geom_point()

## filter data
dataset <- dataset[dataset$PRICE<195000000,]

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

dataset <- dataset[dataset$BEDS <25,]

## fit linear model
lmod <- lm(log10(PRICE)~log10(PROPERTYSQFT)+BEDS, data = dataset)

## print model output
summary(lmod)

Call:
  lm(formula = log10(PRICE) ~ log10(PROPERTYSQFT) + BEDS, data = dataset)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.06499 -0.19260 -0.04337  0.17640  1.03766 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)          2.235490   0.075071  29.778  < 2e-16 ***
  log10(PROPERTYSQFT)  1.193388   0.025812  46.233  < 2e-16 ***
  BEDS                -0.021656   0.003574  -6.059 1.63e-09 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2814 on 2069 degrees of freedom
Multiple R-squared:  0.6165,	Adjusted R-squared:  0.6162 
F-statistic:  1663 on 2 and 2069 DF,  p-value: < 2.2e-16

## scatter plot of 2 variables
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod)
plot(log10(PRICE)~BEDS, data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = log10(PROPERTYSQFT)+BEDS, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="purple")




##Linear model 3: PropertySqFt & Beds & Bath vs Price 
ggplot(dataset, aes(x = log10(PROPERTYSQFT) + (BEDS) + (BATH), y = log10(PRICE))) +
  geom_point()

## filter data
dataset <- dataset[dataset$PRICE<195000000,]
dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]
dataset <- dataset[dataset$BEDS <15,]
dataset <- dataset[dataset$BATH <10,]
ggplot(dataset, aes(x = (BATH) + (BEDS), y = log10(PRICE))) +
  geom_point()

## fit linear model
lmod <- lm(log10(PRICE)~log10(PROPERTYSQFT)+BEDS+BATH, data = dataset)

## print model output
summary(lmod)

Call:
  lm(formula = log10(PRICE) ~ log10(PROPERTYSQFT) + BEDS + BATH, 
     data = dataset)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.98654 -0.18953 -0.03629  0.17722  0.97411 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)          2.588026   0.083186  31.111   <2e-16 ***
  log10(PROPERTYSQFT)  1.054654   0.029600  35.631   <2e-16 ***
  BEDS                -0.038279   0.004092  -9.354   <2e-16 ***
  BATH                 0.061589   0.006547   9.407   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2759 on 2062 degrees of freedom
Multiple R-squared:  0.632,	Adjusted R-squared:  0.6315 
F-statistic:  1181 on 3 and 2062 DF,  p-value: < 2.2e-16

## scatter plot of 2 variables
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod)
plot(log10(PRICE)~BEDS, data = dataset)
abline(lmod)
plot(log10(PRICE)~BATH, data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = log10(PROPERTYSQFT)+BEDS+BATH, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="pink")


