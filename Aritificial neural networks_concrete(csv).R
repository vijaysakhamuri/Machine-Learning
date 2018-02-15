##evaluating performance by ANN(artificial neural networks) concrete dataset 

concrete <- read.csv("C:/Users/ViJaY PaVan/Documents/data sets/Machine-Learning-with-R-datasets-master/concrete.csv")

str(concrete) 

##function for normalising the data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
} 

##applying normalise function to all the columns to concrete data
concrete_norm <- as.data.frame(lapply(concrete, normalize))

##we can observe the normalising differences of normalised concrete data and initial data 
summary(concrete_norm$strength) 

summary(concrete$strength)

##divide the data into test and train sets.
concrete_train <- concrete_norm[1:773, ]

concrete_test <- concrete_norm[774:1030, ] 

##training the model....install neuralnet package
install.packages("neuralnet") 

library(neuralnet) 

concrete_model <- neuralnet(strength ~ cement + slag +ash + water 
                            + superplastic +coarseagg + fineagg + age,
                            data = concrete_train)

plot(concrete_model)

##evaluating model performance 
model_results <- compute(concrete_model, concrete_test[1:8])

##compute() function works a bit differently from the predict() functions
##we've used so far. It returns a list with two components: $neurons, which stores the neurons for each layer in the network,
##and $net.results, which stores the predicted values

predicted_strength <- model_results$net.result

cor(predicted_strength, concrete_test$strength) 

##improving model performance by changing the hidden nodes 
concrete_model2 <- neuralnet(strength ~ cement + slag +                               
                               ash + water + superplastic +                                
                               coarseagg + fineagg + age,                               
                             data = concrete_train, hidden = 3)
plot(concrete_model2)

##evaluating the model performance
model_results2 <- compute(concrete_model2, concrete_test[1:8]) 

predicted_strength2 <- model_results2$net.result 

cor(predicted_strength2, concrete_test$strength) 

concrete_model3 <- neuralnet(strength ~ cement + slag +                               
                               ash + water + superplastic +                                
                               coarseagg + fineagg + age,                               
                             data = concrete_train, hidden = 5)
plot(concrete_model3)

##evaluating the model performance
model_results3 <- compute(concrete_model3, concrete_test[1:8]) 

predicted_strength3 <- model_results3$net.result 

cor(predicted_strength3, concrete_test$strength) 