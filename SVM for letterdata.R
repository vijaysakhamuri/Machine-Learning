##SVM for letterdata

##load data
letters <- read.csv("C:/Users/ViJaY PaVan/Documents/data sets/Machine-Learning-with-R-datasets/letterdata.csv") 

##str
str(letters)

## divide the data into test and train
letters_train <- letters[1:16000, ] 
letters_test  <- letters[16001:20000, ]

##install kernlab package
library(kernlab)

##building model with linear classifier 
letter_classifier <- ksvm(letter ~ ., data = letters_train,kernel = "vanilladot") 

letter_classifier

##evaluating model performance 
letter_predictions <- predict(letter_classifier, letters_test)

head(letter_predictions) 

table(letter_predictions, letters_test$letter) 

agreement <- letter_predictions == letters_test$letter

table(agreement)

prop.table(table(agreement))

## improving model performance with  Gaussian RBF kernel
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train,kernel = "rbfdot") 

letter_predictions_rbf <- predict(letter_classifier_rbf,letters_test) 

agreement_rbf <- letter_predictions_rbf == letters_test$letter

table(agreement_rbf) 

prop.table(table(agreement_rbf)) 
