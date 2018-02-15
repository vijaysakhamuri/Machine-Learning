##linear regression :y=b+ax
##loading the data

    launch<-read.csv('C:/Users/ViJaY PaVan/Documents/data sets/Machine-Learning-with-R-datasets-master/challenger.csv')

##vewing the data 

    launch

##calculate the values of eq: by cov() and mean(),var()

    b <- cov(launch$temperature,launch$distress_ct)/var(launch$temperature) 

    a <- mean(launch$distress_ct) - b * mean(launch$temperature) 
  
##corelation between the variables ....here we call our sd() and cov()
  
    r <- cov(launch$temperature,launch$distress_ct)/(sd(launch$temperature) * sd(launch$distress_ct)) 
  
##regression function
    
      reg <- function(y, x) {    
      x <- as.matrix(x)    
      x <- cbind(Intercept = 1, x)    
      solve(t(x) %*% x) %*% t(x) %*% y  
      } 
    
##single variable
      reg(y = launch$distress_ct, x = launch[3])
      
##multiple variable      
    reg(y = launch$distress_ct, x = launch[3:5])
  
