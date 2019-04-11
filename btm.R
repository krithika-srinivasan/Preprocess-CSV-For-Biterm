library(BTM)
library(tidyr)
library(dplyr)
library(tidytext)
library(tm)
library(data.table)

#Read the data
#txtdata <- read.csv(file = "panel_info.csv",fileEncoding = "utf8",quote = F)
read_simple_user_info_w_nulls <- function(inFile){
  
  r = readBin(inFile, raw(), file.info(inFile)$size)
  r[r==as.raw(0)] = as.raw(0x20)
  tfile = tempfile(fileext=".txt")
  writeBin(r, tfile)
  rm(r)
  inFile = tfile
  
  return(read_simple_user_info(inFile))
}
txtdata <- txtdata$text

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
#txtdata <- tibble(line = 1:35434, text = txtdata)
tidytxtdata<- tidy(txtdtm)
tidytxtdata <- txtdata %>%
  unnest_tokens(word, term)

tidytxtdata <- tidytxtdata%>%
  select(-count)
tidytxtdata <- tidytxtdata%>%
  rename(word = term)


#Remove stopwords
tidytxtdata <- tidytxtdata%>%
  anti_join(stop_words)


#Use the btm model
set.seed(321)
model  <- BTM(tidytxtdata, k = 10, beta = 0.001, iter = 1000, trace = 100) 

topicterms <- terms(model, top_n = 10)
topicterms
