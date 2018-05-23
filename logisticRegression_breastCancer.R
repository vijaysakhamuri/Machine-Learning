##load mlbench package for breastCancer dataset
library(mlbench)
#load data
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ]

#remove id column
bc <- bc[,-1]

#check the distrubution of data
summary(bc)
#check the feel of data
head(bc)
str(bc)

#check for missing values
colSums(is.na(bc))

#creating the model
model<-glm(Class ~ Cell.shape, family="binomial", data = bc)

#call model
model

#convert factors to numeric because there are some ord.factors
for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}
#convert the response variable into binomial factor
bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))

#know the partitions of the class
table(bc$Class)

#install caret package for data partitioning
library(lattice)
library(ggplot2)
library(caret)
#creat a function for "not in"
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.

set.seed(100)
trainDataIndex <- createDataPartition(bc$Class, p=0.7, list = F)  # 70% training data
trainData <- bc[trainDataIndex, ]
testData <- bc[-trainDataIndex, ]

table(trainData$Class)

# Down Sample the data because the distribution is 2times than the other sample
set.seed(100)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"],
                         y = trainData$Class)

table(down_train$Class)

## Up Sample the data
set.seed(100)
up_train <- upSample(x = trainData[, colnames(trainData) %ni% "Class"],
                     y = trainData$Class)

table(up_train$Class)

# Building Logistic Model
logit_mod <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape, family = "binomial", data=down_train)

summary(logit_mod)

#test the model
pred <- predict(logit_mod, newdata = testData, type = "response")

y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class

mean(y_pred == y_act) 

##here we have achieved +94% accuracy