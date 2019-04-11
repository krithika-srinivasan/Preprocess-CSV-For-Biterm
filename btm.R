library(BTM)
library(tidyr)
library(dplyr)
library(tidytext)
library(tm)
library(data.table)

#Read the data
txtdata <- read.csv(file = "[your csv filename].csv")

txtdata <- txtdata$text #Or whatever the header of the column containing the text is

txtcorpus <- Corpus(VectorSource(txtdata))
#txtcorpus

#Clean the corpus
txtcorpus <- tm_map(txtcorpus, stripWhitespace)
txtcorpus <- tm_map(txtcorpus, removePunctuation)
txtcorpus <- tm_map(txtcorpus, removeNumbers)
txtcorpus <- tm_map(txtcorpus, tolower)
txtcorpus <- tm_map(txtcorpus, function(x) iconv(enc2utf8(x), sub = "byte")) #Remove emojis

#Make a document term matrix
txtdtm <- DocumentTermMatrix(txtcorpus)
txtdtm

#Tokenize data
tidytxtdata<- tidy(txtdtm)
                    
tidytxtdata <- tidytxtdata%>% #Remove the count column
  select(-count)
tidytxtdata <- tidytxtdata%>% #Change the column name 'term' to 'word' so that we can get rid of stopwords later
  rename(word = term)


#Remove stopwords
tidytxtdata <- tidytxtdata%>%
  anti_join(stop_words)


#Use the btm model
set.seed(321)
model  <- BTM(tidytxtdata, k = 10, beta = 0.001, iter = 1000, trace = 100) #Run the model

topicterms <- terms(model, top_n = 10) #View the topics
topicterms
