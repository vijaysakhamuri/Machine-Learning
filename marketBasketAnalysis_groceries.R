##market basket analysis by APRIORI algo.
##load data
setwd("C:/Users/ViJaY PaVan/Documents/data sets/Machine-Learning-with-R-datasets")
groceries <- read.csv("groceries.csv")
##load arules package
library(arules)
groceries <- read.transactions("groceries.csv", sep = ",")

summary(groceries)
head(groceries)
str(groceries)
groceries[1:5]

##view the items frequency
inspect(groceries[1:3])
itemFrequency(groceries[, 1:3]) 

##visualise the items in grocery
itemFrequencyPlot(groceries, support = 0.01) 
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, support = 0.005) 
itemFrequencyPlot(groceries, topN = 20) 
image(groceries[1:35]) 
image(sample(groceries, 100))

##creating model
apriori(groceries)
groceryrules <- apriori(groceries, parameter = list(support =0.006, confidence = 0.25, minlen = 2))
groceryrules 

summary(groceryrules) 
inspect(groceryrules[50:100])
#improving model performance
inspect(sort(groceryrules, by = "lift")[1:5]) 

##Taking subsets of association rules
berryrules <- subset(groceryrules, items %in% "berries") 
inspect(berryrules)

##Saving association rules to a file or data frame 
write(groceryrules, file = "groceryrules.csv",sep = ",", quote = TRUE, row.names = FALSE)
groceryrules_df <- as(groceryrules, "data.frame") 
str(groceryrules_df)