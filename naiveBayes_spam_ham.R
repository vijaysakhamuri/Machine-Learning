##naive bayes for spam classifier
##load the data
setwd("C:/Users/ViJaY PaVan/Documents/data sets/Machine-Learning-with-R-datasets")
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE) 
str(sms_raw)

##convert type variable into factor....for ease analysis(into levels)
sms_raw$type <- factor(sms_raw$type) 
str(sms_raw$type)
table(sms_raw$type)

##install TM package for text processing
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus) 
inspect(sms_corpus[1:10])

##convert all words into lower cases and remove letters for fast processing
corpus_clean <- tm_map(sms_corpus, tolower) 
corpus_clean <- tm_map(corpus_clean, removeNumbers) 

##remove stop words("but","or","and","the","or") and remove punctuations 
##and remove whitespaces
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords()) 
corpus_clean <- tm_map(corpus_clean, removePunctuation) 
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
inspect(corpus_clean[1:10])

##now perform tokenization like one-hot encoding
sms_dtm <- DocumentTermMatrix(corpus_clean)

##model creation
#raw data
sms_raw_train <- sms_raw[1:4169, ] 
sms_raw_test  <- sms_raw[4170:5559, ]

#document-term matrix
sms_dtm_train <- sms_dtm[1:4169, ] 
sms_dtm_test  <- sms_dtm[4170:5559, ] 

#the corpus
sms_corpus_train <- corpus_clean[1:4169] 
sms_corpus_test  <- corpus_clean[4170:5559] 

#check the proportions
prop.table(table(sms_raw_train$type)) 
prop.table(table(sms_raw_test$type)) 

##install wordcloud package for visualising
library(wordcloud)
wordcloud(sms_corpus_train, min.freq = 40, random.order = FALSE) 

##comparision b/w ham and spam
spam <- subset(sms_raw_train, type == "spam") 
ham <- subset(sms_raw_train, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5)) 
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

##save the list of freq words
sms_dict <- findFreqTerms(sms_dtm_train, 5)
sms_train <- DocumentTermMatrix(sms_corpus_train,list(dictionary = sms_dict)) 
sms_test  <- DocumentTermMatrix(sms_corpus_test,list(dictionary = sms_dict)) 

##creating a function to covert levels into lables
convert_counts<- function(x){
  x<- ifelse(x>0,1,0)
x<-factor(x,levels = c(0,1),labels = c("No","Yes"))
return(x)}

sms_train <- apply(sms_train, MARGIN = 2, convert_counts) 
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts) 

##training the model
#install e1071 package for performing navie bayes
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_test_pred <- predict(sms_classifier, sms_test)

#cross table
library(gmodels) 
CrossTable(sms_test_pred, sms_raw_test$type,prop.chisq = FALSE, 
           prop.t = FALSE,dnn = c('predicted', 'actual')) 

#improve model performance by laplace transformer
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type,laplace = 1) 
sms_test_pred2 <- predict(sms_classifier2, sms_test) 
CrossTable(sms_test_pred2, sms_raw_test$type, prop.chisq = FALSE, 
           prop.t = FALSE, prop.r = FALSE,dnn = c('predicted', 'actual')) 