library(mlbench)
library(e1071)
library(tm)
library(gmodels)
library(caret)
library(dplyr)
install.packages("rmarkdown")
library(rmarkdown)
sms_data <- read.csv(choose.files(),stringsAsFactors = F)###sms data
class(sms_data)
str(sms_data)
sms_data$type <- as.factor(sms_data$type)
table(sms_data$type)
plot(sms_data$type)

###prepare corpus for the text data
###Vcorpus is a volatile corpus which stores the corpusin memory 
sms_corpus <- VCorpus(VectorSource(sms_data$text))
class(sms_corpus)

##cleaning the data
corpus_clean <- tm_map(sms_corpus,tolower)
corpus_clean <- tm_map(corpus_clean,removeNumbers)
corpus_clear <- tm_map(corpus_clean,removeWords,stopwords())                      
corpus_clean <- tm_map(corpus_clean,removePunctuation)
corpus_clean <- tm_map(corpus_clean,stripWhitespace)
corpus_clean <- tm_map(corpus_clean,PlainTextDocument)
class(corpus_clean)

###create a document-term matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)
class(sms_dtm)

##creating training and test data
sms_raw_train <- sms_data[1:4169,]
sms_raw_test <- sms_data[4170:5559,]

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5559]

####check the proportion of spam is similar 
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

###if the word has been referred to 5 times or more
sms_dict <- findFreqTerms(sms_dtm,5)
sms_dtm

sms_train <- DocumentTermMatrix(sms_corpus_train,list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test,list(dictionary = sms_dict))

temp <- as.data.frame(as.matrix(sms_train))
View(temp)
dim(sms_train)
dim(sms_test)

convert_count <- function(x){
  x <- ifelse(x > 0,1,0)
  x <- factor(x,levels = c(0,1),labels = c("No","Yes"))
}

sms_train <- apply(sms_train,MARGIN = 2,convert_count)
View(sms_train)
sms_test <- apply(sms_test,MARGIN = 2,convert_count)
View(sms_test)
sms_NBmodel <- naiveBayes(sms_train,sms_raw_train$type)
sms_NBmodel

##prediction
pred <- predict(sms_NBmodel,sms_test)
CrossTable(pred,sms_raw_test$type)
confusionMatrix(pred,sms_raw_test$type)
