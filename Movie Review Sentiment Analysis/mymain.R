start.time = proc.time()

# Install and load required packages
installIfNeeded = function(cliblist){
  libsNeeded = cliblist
  libsNeeded = libsNeeded[!(libsNeeded %in% installed.packages()[,"Package"])]
  if(length(libsNeeded)>0) install.packages(libsNeeded)
}

installIfNeeded(c('tm', "tidytext", "stringr", "pROC"))

library(tm)
library(stringr)
library(tidytext)
library(glmnet)
library(pROC)

# Read in the data 
data <- read.table("data.tsv", header = T)
splits <- read.csv("splits.csv", sep = "\t")

data$review <- as.character(data$review)

# Splitting the data into test train
s = 1
train = data[-which(data$new_id%in%splits[,s]),]
test = data[which(data$new_id%in%splits[,s]),]

# Separating the test and train labels out
train_labels <- train$sentiment
test_labels <- test$sentiment

train$sentiment <- NULL
test$sentiment <- NULL

# Function to create BOW from the reviews
freq_word <- function(document.vector, sparsity = .995)
{
  # construct corpus
  temp.corpus <- Corpus(VectorSource(document.vector))
  # construct tf matrix and remove sparse terms
  temp.tf <- DocumentTermMatrix(temp.corpus, control = list(stopwords = stopwords("english"), removeNumbers = T))
  
  temp.tf <- removeSparseTerms(temp.tf, sparsity)
  temp.tf <- as.matrix(temp.tf)
  # construct word frequency data
  freq.data <- colSums(temp.tf)
  freq.data <- data.frame(word = names(freq.data), freq = freq.data)
  rownames(freq.data) <- NULL
  return(freq.data)
}

# Create word freq for positive and negative reviews
word_freq_pos <- freq_word(train$review[train_labels == 1])
word_freq_neg <- freq_word(train$review[train_labels == 0])

# merge the word freq for positive and negative reviews
all_freq <- merge(word_freq_pos, word_freq_neg, by = "word", all = T)

# replace NA values with 0s
all_freq$freq.x[is.na(all_freq$freq.x)] <- 0
all_freq$freq.y[is.na(all_freq$freq.y)] <- 0

# compute difference between word freq of positive and negative reviews
all_freq$diff <- abs(all_freq$freq.x - all_freq$freq.y)

# Calculating ndsi
alpha = 2**6
all_freq$ndsi <- abs(all_freq$freq.x - all_freq$freq.y)/(all_freq$freq.x + all_freq$freq.y + 2*alpha)

# sorting the frequency data by ndsi
all_freq <- all_freq[order(-all_freq$ndsi), ]

# convert word to string
all_freq$word <- as.character(all_freq$word)

# build the tf matrix
num_features = 3000
all_data <- rbind.data.frame(train, test)

term_freq <- t(apply(t(all_data$review), 2,
                     function(x) str_count(x, all_freq$word[1:num_features])))

inverse_doc_freq <- log(length(train_labels)/colSums(sign(term_freq[c(1:length(train_labels)), ])))
inverse_doc_freq[is.infinite(inverse_doc_freq)] <- 0

# tf-idf matrix
tf_idf <- as.data.frame(t(apply(term_freq, 1, function(x) x * inverse_doc_freq)))
colnames(tf_idf) <- all_freq$word[1:num_features]

dtm_train <- tf_idf[c(1:length(train_labels)),]
dtm_test <- tf_idf[-c(1:length(train_labels)),]
dtm_train <- model.matrix(train_labels ~ ., data = data.frame(dtm_train))
dtm_test <- model.matrix(train_labels ~ ., data = data.frame(dtm_test))


NFOLDS = 5
mycv = cv.glmnet(x=dtm_train, y=train_labels, 
                 family='binomial',type.measure = "auc", 
                 nfolds = NFOLDS, alpha=0)

myfit = glmnet(x=dtm_train, y=train_labels, 
               lambda = mycv$lambda.min, family='binomial', alpha=0)

pred = predict(myfit, dtm_test, type = "response")

roc_obj = roc(test_labels, as.vector(pred))
auc(roc_obj)

write.table(matrix(data=c(test$new_id,pred),ncol=2),file="mysubmission.txt",row.names=FALSE,col.names=c("id","prob"),sep=',')


run_time = proc.time() - start.time
