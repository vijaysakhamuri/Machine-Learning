
wine<- read.csv("C:/Users/ViJaY PaVan/Documents/data sets/Machine-Learning-with-R-datasets-master/whitewines.csv")

str(wine)

hist(wine$quality) 

wine_train <- wine[1:3750, ] 

wine_test <- wine[3751:4898, ] 

##install rpart package for regression tree modelling
library(rpart)

m.rpart <- rpart(quality ~ ., data = wine_train) 

m.rpart 
summary(m.rpart)

##install rpart.plot for visualizing
library(rpart.plot)

rpart.plot(m.rpart, digits = 3) 

rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,type = 3, extra = 101) 

##predictions on the test data
p.rpart <- predict(m.rpart, wine_test)

summary(p.rpart)
summary(wine_test$quality)
 
##corelation between variables because we didn't found the similarities by the summary

cor(p.rpart, wine_test$quality) 

##Measuring performance with mean absolute error 

MAE <- function(actual, predicted) {    mean(abs(actual - predicted))  }

MAE(p.rpart, wine_test$quality) 

mean(wine_train$quality) 
mean_abserror(5.88, wine_test$quality) 

##install Rweka package for improvingthe model ....m5' algorithm
library(JavaGD)
library(rJava)
library(namespace)
library(RWeka)

m.m5p <- M5P(quality ~ ., data = wine_train)

m.m5p 

summary(m.m5p)