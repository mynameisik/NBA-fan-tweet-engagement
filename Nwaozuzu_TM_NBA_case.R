#' Title: Case 1- NBA fan tweet engagement
#' NAME: Ikechi Nwaozuzu
#' Date: March 13 2022
#' Text analytics case study
#######################################
#Loading packages, working directory and reading CSV file
#######################################
library(dplyr) # Load dplyr package
library(readr) # To import csv file


#set working directory
setwd("C:/Users/ikech/OneDrive/Desktop/Text-Mining-NLP/Case/Case I/Data")


#file workload of entire corpora was large, could not process with system size.
#providing analysis on first month.
NBA <-  read_csv("A_Oct2019.csv")

#analyzing the tweets at the beginning of the season to see whats being said


library(plyr)
library(tidyverse)    # for data wrangling and visualization
library(tm)
library(ggplot2)
library(ggthemes)
library(stringi)   #for string manipulation
library(stringr)   #additional string manipulation
library(qdap)    
library(tidytext)
library(RColorBrewer)


# Options & Functions
options(stringsAsFactors=F) 
#to properly analyze text, we need to make sure the texts are not in factor form
#the code above clears that up
Sys.setlocale('LC_ALL','C')
#this sets the IDE language to the system language ; English


##################################
#loading basic cleaning functions
#trytolower - turns all characters to lower case
#cleancorpus - custom UDF to apply basic cleaning operations on text
#basicsubs - custom UDF added as an additional option to preprocess text
##################################
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

basicSubs <- function(x){
  x <- gsub('http\\S+\\s*', '', x)
  x <- gsub('(\\bRT\\b|via)((?:\\b\\W*@\\w+)+)', '', x)
  x <- gsub("[^\x01-\x7F]", '', x)
  x <- str_replace_all(x, "[[:punct:]]", " ")
  x <- tolower(x)
  return(x)
}
################################

#calling data set to view observations

head(NBA,3)


##################################
#due to the large corpus and system failure, a subset of
#the dataset will be obtained, 20% of the data was obtained
#to maintain representation of the whole, dataset was subsetted 
#by dates to obtain random tweets though october for all teams
##################################
library(splitstackshape) 
#to use in obtaining a representative sample from data set

nba_idx <- stratified(indt= NBA, size = 0.80,
                           group = 3,bothSets = TRUE)

newNBA <- nba_idx$SAMP2 
#extract (20%) a small stratified sample based on dates

##################################
#Data cleaning and pre-processing operations
##################################
newNBA <- subset(newNBA,nchar(newNBA$text)>0)
#sub-setting dataset for only observations that have text in them

newNBA$text <- basicSubs(newNBA$text)
#applying basic subs to remove @,RTs and other unneeded characters
newNBA <- as.data.frame(newNBA)

newNBA[1] <- 1:nrow(newNBA) #renaming doc_id variables

newNBA <- newNBA[-c(3:4)] # removing unneeded columns

head(newNBA,2)


#creating custom stopwords, theses words were generated from tweet general lingo
#and basketball slangs that were noticed throughout the dataset
custom_stopwords <- c(stopwords('SMART'), stopwords('english'), 'lol','smh', 
                      'fans', 'nba', 'games', 'night', 'league', 'pts',
                      'twitter', 'nbatwitter', 'team', 'us', 'game', 'season',
                      'basketball', 'retweet', 'day', 'yall', 'good','nbd','nvm',
                      'lmk', 'kk', 'obvi', 'obv', 'srsly', 'btw', 'usa',
                      'rly', 'tmi', 'ty', 'tyvm', 'yw', 'fomo', 'ftw', 
                      'icymi', 'icyww', 'ntw', 'omg', 'omfg', 'idk', 'idc', 
                      'jell', 'iirc', 'ffs', 'fml', 'idgaf', 'stfu', 'tf',
                      'omw', 'rn', 'ttyl', 'tyt', 'bball','oct','october')

# length(nchar(grep("smh",newNBA$text)))
#using the above code to randomly check for possibility of the existence of
#tweets slang, or repetitive unneeded words

txtCorpus <- VCorpus(DataframeSource(newNBA)) #making a volatile corpus

# Preprocess the corpus, considering the provided stopwords
txtCorpus <- cleanCorpus(txtCorpus, custom_stopwords)

# Checking Meta Data info
txtCorpus[[3]]
meta(txtCorpus[3])
content(txtCorpus[[3]])

#making a termdocument matrix 
tweetTDM  <- TermDocumentMatrix(txtCorpus)
tweetTDMm <- as.matrix(tweetTDM)

#sorting data frame from highest to lowest to observe highest word count
#or most common topics
topTerms <- rowSums(tweetTDMm)
topTerms <- data.frame(word = names(topTerms), 
                       freq = topTerms)

#removing row names to give a cleaner output
rownames(topTerms) <- NULL

topTerms <- topTerms[order(topTerms$freq, decreasing = T),]

  head(topTerms)

# Which term is the most frequent?
# idx <- which.max(topTerms$freq)
# topTerms[idx, ]



#######################################
#plotting word count visualizations
#######################################

topTerms$word<-factor(topTerms$word,
                     levels=unique(as.character(topTerms$word)))
ggplot(topTerms[1:20,], aes(x=word,
                           y=freq))+geom_bar(stat="identity",
                                                  fill='darkred')+coord_flip()+theme_gdocs()+
  geom_text(aes(label=freq),
            colour="white",hjust=1.25, size=5.0)


# Make simple word cloud
library(wordcloud)
set.seed(1234)
wordcloud(topTerms$word,
          topTerms$freq,
          max.words    = 100,
          random.order = TRUE,
          colors       = c('darkred','darkblue'),
          scale        = c(2,1))


#######################################
# WORD ASSOCIATION
#######################################
# Inspect word associations
associations <- findAssocs(tweetTDM, 'china', 0.10)
associations

#i noticed this word come up in most frequent items

#https://www.sbnation.com/nba/2019/10/8/20904450/nba-china-fallout-lakers-vs-nets-broadcast-streaming-tencent

ass <- findAssocs(tweetTDM, 'live', 0.11)
ass
# i noticed this word come up in most frequent items

# Organize the word associations
associations <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
associations$terms <- factor(associations$terms, levels=associations$terms)
rownames(associations) <- NULL

mini_ass <- associations[1:10,]
#a smaller plot 

ass <- data.frame(terms=names(ass[[1]]),
                  value=unlist(ass))
ass$terms <- factor(ass$terms, levels=ass$terms)
rownames(ass) <- NULL

# Make dot plots
# dot plot for word association 'china'
###########################
ggplot(associations, aes(y=terms)) +
  geom_point(aes(x=value), data=associations, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label= value), colour="darkred",
            hjust="inward", vjust ="inward" , size=3) 

#minimized version
ggplot(mini_ass, aes(y=terms)) +
  geom_point(aes(x=value), data=mini_ass, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label= value), colour="darkred",
            hjust="inward", vjust ="inward" , size=3) 

#dot plot for word association 'live'
############################
ggplot(ass, aes(y=terms)) +
  geom_point(aes(x=value), data=ass, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label= value), colour="darkred",
            hjust="inward", vjust ="inward" , size=3) 



#######################################
# POLARITY ANALYSIS
#######################################

#grep("slam", x= as.data.frame(key.pol))
#i used the above code to check possible negative/positive words that may not be
#in subjectivity lexicon & appending new words to custom negative/positive 
#lexicon to append
neg_words<-c('tears','meh')

pos_words<-c('rofl','lol')

old_pos<-subset(as.data.frame(key.pol),key.pol$y==1)
old_neg<-subset(as.data.frame(key.pol),key.pol$y==-1)

good_words <- c(pos_words,old_pos[,1])
bad_words <- c(neg_words,old_neg[,1])

words_polarity <- sentiment_frame(good_words,bad_words,1,-1)

#calculating polarity of tweets for the month of october
NBA_polarity <- polarity(removePunctuation(removeNumbers(tolower(newNBA$text))),
                         polarity.frame = words_polarity)
NBA_polarity
#appending results to nba dataset
newNBA$polarity <- scale(NBA_polarity$all$polarity)

#as a precaution, subset once again for only tweets with texts
newNBA <- subset(newNBA,nchar(newNBA$text)>0)

#make a subset of all good tweets and comments/bad tweets and comments  
good_comments <- subset(newNBA$text,
                        newNBA$polarity>0)
bad_comments <- subset(newNBA$text,
                       newNBA$polarity<0)

#preprocessing steps to create a polarity word cloud
pos.terms<-paste(good_comments,collapse = " ")
neg.terms<-paste(bad_comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))

all.tdm<-TermDocumentMatrix(all.corpus,control=list(weighting=weightTfIdf,
                                                    removePunctuation = TRUE,
                                                    stopwords=custom_stopwords))


all.tdm.m<-as.matrix(all.tdm)

#naming column values
colnames(all.tdm.m)<-c('positive','negative')

#plotting comparison cloud to see postive and negative top terms 
comparison.cloud(all.tdm.m, max.words=100,
                 random.order = TRUE,
                 colors=c('darkred','darkblue'))

