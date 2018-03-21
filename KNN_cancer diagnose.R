##KNN for Diagnosing breast cancer 

##set working directory
setwd("C:/Users/ViJaY PaVan/Documents/data sets/Machine-Learning-with-R-datasets")

##load data
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE) 

str(wbcd)

##exclude the 1st column(id), we don't have any use while implementing algorithm
##ID is unique for each example,it won't be useful in comparison of variables

wbcd <- wbcd[-1]

##our prediction
table(wbcd$diagnosis) 

##we have only one categorical variable and remaining all were the numeric variables
##we use factors for that particular varaiable

wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant")) 

##probabilty percentage for the $diagnosis variable

round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1) 

##summary of 3varaiables,Here we have a large n0 of variables
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")]) 

##Function for normalising the variables
normalize <- function(x) {    
  return ((x - min(x)) / (max(x) - min(x)))  }

##check the function whether it is working fine or not
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50)) 

##normalise the whole dataframe by lapply(list apply) and normalise function
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

summary(wbcd_n$area_mean)

##divide the data into test and train
wbcd_train <- wbcd_n[1:469, ] 
wbcd_test <- wbcd_n[470:569, ]

wbcd_train_labels <- wbcd[1:469, 1] 
wbcd_test_labels <- wbcd[470:569, 1] 

##install class package for using the KNN ecludian distance formula
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,                        
                      cl = wbcd_train_labels, k=21) 

library(gmodels) 
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,           
           prop.chisq=FALSE) 

##improving the accuracy by Z-score standardisation
wbcd_z <- as.data.frame(scale(wbcd[-1])) 
summary(wbcd_z$area_mean) 

##applying the model
  wbcd_train1 <- wbcd_z[1:469, ]  
  wbcd_test1 <- wbcd_z[470:569, ] 
  
  wbcd_train_labels <- wbcd[1:469, 1] 
  wbcd_test_labels <- wbcd[470:569, 1] 
  
  wbcd_test_pred1 <- knn(train = wbcd_train1, test = wbcd_test1,                        
                        cl = wbcd_train_labels, k=21)
  
  CrossTable(x = wbcd_test_labels, y = wbcd_test_pred1,
                           prop.chisq=FALSE)