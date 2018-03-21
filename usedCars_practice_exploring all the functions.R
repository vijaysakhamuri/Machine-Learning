##sample practice

usedCars <- read.csv('C:/Users/ViJaY PaVan/Documents/data sets/Machine-Learning-with-R-datasets-master/usedcars.csv')

str(usedcars)

summary(usedcars$year)

summary(usedcars[c("price", "mileage")]) 

mean(c(36000, 44000, 56000))
median(c(36000, 44000, 56000))

range(usedcars$price) 
diff(range(usedcars$price))

##quantiles
IQR(usedcars$price) 
quantile(usedcars$price)
quantile(usedcars$price, probs = c(0.01, 0.99)) 
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20)) 

##visualisation with boxplots
boxplot(usedcars$price, main="Boxplot of Used Car Prices",ylab="Price ($)")
boxplot(usedcars$mileage, main="Boxplot of Used Car Mileage",ylab="Odometer (mi.)")

##visualisation with boxplots
hist(usedcars$price, main = "Histogram of Used Car Prices",xlab = "Price ($)")
hist(usedcars$mileage, main = "Histogram of Used Car Mileage",xlab = "Odometer (mi.)")

##variance and standard deviation 
var(usedcars$price) 
sd(usedcars$price) 

##exploring categorial variables in table structures
table(usedcars$year) 
table(usedcars$model)
table(usedcars$color)
##probabilities
model_table <- table(usedcars$model) 
prop.table(model_table) 

##probabilities in percentages
color_table <- table(usedcars$color) 
color_pct <- prop.table(color_table) * 100 
round(color_pct, digits = 1) 

##scater plot for visualizing the bi-variant analysis 
##with negative association because the plot shows the decrease according to the axis 
plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi.)",ylab = "Used Car Price ($)")

##Examining relationships - two-way  cross-tabulations 
install.packages("gmodels") 
library(gmodels)

usedcars$conservative<-usedcars$color %in% c("Black", "Gray", "Silver", "White")
table(usedcars$conservative) 

CrossTable(x = usedcars$model, y = usedcars$conservative) 
CrossTable(x = usedcars$model, y = usedcars$conservative,chisq = TRUE)