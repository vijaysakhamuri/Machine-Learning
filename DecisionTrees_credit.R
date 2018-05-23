
setwd("C:/Users/ViJaY PaVan/Documents/data sets/Machine-Learning-with-R-datasets")
credit <- read.csv('credit.csv')
str(credit)
credit_train$default<-as.factor(credit_train$default)
table(credit$checking_balance)
table(credit$savings_balance) 
summary(credit$months_loan_duration)
summary(credit$amount) 
table(credit$default) 
set.seed(12345) 
credit_rand <- credit[order(runif(1000)), ] 
summary(credit$amount) 
summary(credit_rand$amount)
head(credit$amount) 
head(credit_rand$amount) 
credit_train <- credit_rand[1:900, ] 
credit_test  <- credit_rand[901:1000, ] 
prop.table(table(credit_train$default)) 
prop.table(table(credit_test$default))
library(C50)
credit_model <- C5.0 (credit_train[-17], credit_train$default)
credit_model
summary(credit_model) 
credit_pred <- predict(credit_model, credit_test) 
library(gmodels) 
CrossTable(credit_test$default, credit_pred,             
            prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
            dnn = c('actual default', 'predicted default')) 
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10) 
credit_boost10
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test) 
CrossTable(credit_test$default, credit_boost_pred10,             
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,             
           dnn = c('actual default', 'predicted default'))


