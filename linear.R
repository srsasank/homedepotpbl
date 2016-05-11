library(readr)
library(caret)
cat("Reading data\n")
train <- read_csv('train.csv')
test <- read_csv('test.csv')
desc <- read_csv('product_descriptions.csv')
attrb <- read_csv('attributes.csv')

cat("Merge description with train and test data \n")
train <- merge(train,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
test <- merge(test,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
train$attr<-""
 a=""
 b=""
 train$brand = attrb[df_attr.name == "MFG Brand Name"][["product_uid", "value"]].rename(columns={"value": "brand"})
 
uniqueid <-unique(attrb$product_uid)
unique2<-uniqueid[1:2]
for(i in unique2)
{
 
  b=paste(attrb$value[attrb$product_uid==i]," ",b)
  cat(b)
  train$attr[train$product_uid==i]<-b
 

}

train$attr[1:3]

t <- Sys.time()
word_match <- function(words,title,desc){
  n_title <- 0
  n_desc <- 0
  words <- unlist(strsplit(words," "))
  nwords <- length(words)
  for(i in 1:length(words)){
    pattern <- paste("(^| )",words[i],"($| )",sep="")
    n_title <- n_title + grepl(pattern,title,perl=TRUE,ignore.case=TRUE)
    n_desc <- n_desc + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
  }
  return(c(n_title,nwords,n_desc))
}

cat("Get number of words and word matching title in train\n")
train_words <- as.data.frame(t(mapply(word_match,train$search_term,train$product_title,train$product_description)))
train$nmatch_title <- train_words[,1]
train$nwords <- train_words[,2]
train$nmatch_desc <- train_words[,3]

cat("Get number of words and word matching title in test\n")
test_words <- as.data.frame(t(mapply(word_match,test$search_term,test$product_title,test$product_description)))
test$nmatch_title <- test_words[,1]
test$nwords <- test_words[,2]
test$nmatch_desc <- test_words[,3]

rm(train_words,test_words)

cat("A simple linear model on number of words and number of words that match\n")
glm_model <- glm(relevance~nmatch_title+nmatch_desc+nwords,data=train)
test_relevance <- predict(glm_model,test)
test_relevance <- ifelse(test_relevance>3,3,test_relevance)
test_relevance <- ifelse(test_relevance<1,1,test_relevance)

submission <- data.frame(id=test$id,relevance=test_relevance)
write_csv(submission,"benchmark_submission.csv")
print(Sys.time()-t)
for(i in attrb$product_uid)
{
  cat(attrb$name,attrb$value)
  cat("\n")
}
require(caret)
df_test <- read_csv('df_test.csv')
df_train <- read_csv('df_train.csv')
DF <- data.frame(a = 1:3, b = letters[10:12],
                 c = seq(as.Date("2004-01-01"), by = "week", len = 3),
                 stringsAsFactors = TRUE)
data.matrix(DF[1:2])
data.matrix(DF)
trainloss<-df_train[ , -which(names(df_train) %in% c("product_description","product_info"))]
tftrain<- data.frame(df_train$search_term,
  df_train$product_title,df_train$product_description,df_train$product_info)
tftest<- data.frame(df_test$search_term,
                     df_test$product_title,df_test$product_description,df_test$product_info)
mod1<-train(relevance~.,method="glm",data=df_train)
dfcorpustrain = Corpus(VectorSource(tftrain)) 
dfcorpustrain
trainmatrix <-TermDocumentMatrix(dfcorpustrain)

tftest2<-data.frame((df_test$product_description))
tt<-DataframeSource(tftest2)
trainmatrix <-TermDocumentMatrix(tt)
data("crude")
crude
