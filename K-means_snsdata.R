##K-means for sns(high sschool) data

##load the data

setwd("C:/Users/ViJaY PaVan/Documents/data sets/Machine-Learning-with-R-datasets")

teens <- read.csv('snsdata.csv')

str(teens) 

##handling the missing data

table(teens$gender)

table(teens$gender, useNA = "ifany")

prop.table(table(teens$gender, useNA = "ifany"))

summary(teens$age) 

##filling data with NA whose age greaterthan the teen age
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,teens$age, NA) 

##creating dummy variables for unknown gender and female

teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0) 

table(teens$gender, useNA = "ifany")

##Data preparation - imputing missing values 
mean(teens$age)
mean(teens$age, na.rm = TRUE) 

##calculating the mean based on the grad_year
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

##ave() used for returning group of variables in vector based on the parameters
ave_age <- ave(teens$age, teens$gradyear, FUN =function(x) mean(x, na.rm = TRUE)) 

str(ave_age)

teens$age <- ifelse(is.na(teens$age), ave_age, teens$age) 

summary(teens$age)

##prepare the dataframe which needs to be considered
interests <- teens[5:40] 

##apply z-standardization
interests_z <- as.data.frame(lapply(interests, scale))

teen_clusters <- kmeans(interests_z, 5) 

teen_clusters$size
teen_clusters$centers

str(teen_clusters)

teens$cluster <- teen_clusters$cluster
teens[1:5, c("cluster", "gender", "age", "friends")] 

aggregate(data = teens, age ~ cluster, mean) 
aggregate(data = teens, female ~ cluster, mean) 
aggregate(data = teens, friends ~ cluster, mean)