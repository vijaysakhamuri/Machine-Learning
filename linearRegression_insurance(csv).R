##loading the data....where stringasfactors is converting the strings to factors(int)

insurance <- read.csv("C:/Users/ViJaY PaVan/Documents/data sets/Machine-Learning-with-R-datasets-master/insurance.csv", stringsAsFactors = TRUE)

str(insurance) 

##summary of variable Charges

summary(insurance$charges)

##mean value is higher than the median(right-skewed)

hist(insurance$charges) 

##data have been divided into regions because people will differ based on the regions.

table(insurance$region)

##Exploring relationships among  features - the correlation matrix 

cor(insurance[c("age", "bmi", "children", "charges")]) 

##scatterplot to explore the corelation between variables

pairs(insurance[c("age", "bmi", "children", "charges")]) 

##To get clear insights install psych package if you don't have 
##and run the following script 
library(psych)

pairs.panels(insurance[c("age", "bmi", "children", "charges")]) 

##Describing model

ins_model <- lm(charges ~ age + children + bmi + sex +   smoker + region, data = insurance)

## '.' will take the remaining attributes which are not mentioned.
ins_model <- lm(charges ~ ., data = insurance) 
ins_model

##summary....for how depend variables relate to independent variables
summary(ins_model)

##improve model by creating additional variable to the model
insurance$age2 <- insurance$age^2 

insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

## new improving model

ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex +bmi30*smoker + region, data = insurance) 

summary(ins_model2)
