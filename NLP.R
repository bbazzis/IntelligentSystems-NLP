library(dplyr)
library(ggplot2) 
library(gridExtra) 
library(tidytext) 
library(quanteda)
library("quanteda.sentiment")
library("quanteda.textplots")
library("quanteda.textstats")
library(tidyr)

#Data reading and cleaning
data <- read.csv("data/Rihanna.csv", na.strings = c("", "NA"), encoding = "UTF-8")
data$X <- NULL
data$Artist <- NULL
#Establishing single albums for instances with NA values in Album column
data$Album <- data$Album %>% replace_na("Single")
data$Lyric <- trimws(data$Lyric)

data_clean <- data %>% drop_na()
data_clean$Lyric <- trimws(data_clean$Lyric)

#Transforming contractions found
modify_contractions <- function(df){
  df <- gsub("'re", " are", df)
  df <- gsub("'m", " am", df)
  df <- gsub("'d", " would", df)
  df <- gsub("'ve", " have", df) 
  df <- gsub("'s", " is", df)
  df <- gsub("can't", "can not", df)
  df <- gsub("aren't","are not", df)
  df <- gsub("'ll"," will", df)
  df <- gsub("won't","will not", df)
  df <- gsub("in'","ing", df)
  df <- gsub("don't","do not", df)
  df <- gsub("'cause","because", df)
  df <- gsub("y'","you ", df)
  df <- gsub("gonna","going to", df)
  df <- gsub("c amon","come on", df)
  df <- gsub("wanna","want to", df)
  df <- gsub(" im "," i am ", df)
  return(df)
}
data_clean$Lyric <- sapply(data_clean$Lyric, modify_contractions)
specialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
data_clean$Lyric <- sapply(data_clean$Lyric, specialChars)

#2.1 Most used words without removing stop words and special words
corpus <- corpus(data_clean, text_field = "Lyric")
dfm1 <- dfm(tokens(corpus))
topfeatures(dfm1)

#2.1 Most used words removing stop words and special word
words_to_remove <- c("rihanna","ya", "t", "hey", "eh", "da", "oh","ya","pre","da","la","uh","aingt" )
dfm2 <- dfm1 %>% dfm_remove(stopwords("en")) %>% dfm_remove(words_to_remove)
topfeatures(dfm2, n=100)

dfm_trim1<-dfm_trim(dfm2, min_termfreq = 10, verbose = FALSE)
set.seed(100)
#Plots displaying most used words in full discography
textplot_wordcloud(dfm_trim1)
textplot_wordcloud(dfm_trim1, max_words = 75)

#Output showing most used words by album
toks=tokens(corpus)
dftop_by_album <- dfm(toks)
dftop_by_album <- dftop_by_album %>% dfm_remove(stopwords("en")) %>% dfm_remove(words_to_remove)
top_words_by_album<-topfeatures(dftop_by_album, groups = Album)
top_words_by_album

#2.2 Frequency of words
features_dfm <- textstat_frequency(dfm2, n = 100)
features_dfm$feature <- with(features_dfm, reorder(feature, -frequency))
ggplot(features_dfm, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#2.3 Sentiment Analysis
corpus_sent_by_album = corpus(data_clean, text_field = "Lyric")
docid <- paste(data_clean$Album)
docnames(corpus_sent_by_album) <- docid
toks=tokens(corpus_sent_by_album)
#Using AFINN dictionary
toks_sent_by_album <- tokens_lookup(toks, dictionary = data_dictionary_AFINN)
dfsent_by_album <- dfm(toks_sent_by_album)
dfsent_by_album <- dfsent_by_album %>% dfm_remove(stopwords("en")) %>% dfm_remove(words_to_remove)
dict_output_by_album <- convert(dfsent_by_album, to = "data.frame")
dict_output_by_album$doc_id <- dfsent_by_album@docvars[["Album"]]
#Grouping by Album and combining scores
sum_by_album <- data.frame(aggregate(afinn ~ doc_id, dict_output_by_album, sum))
sum_by_album <- sum_by_album[order(sum_by_album$afinn, decreasing = TRUE),]
#Plot of 10 best scores grouped by album
ggplot(data = head(sum_by_album,10), aes(x = doc_id, y = afinn)) +
  geom_point()
#Plot of 30 best scores grouped by album
ggplot(data = head(sum_by_album,10), aes(x = doc_id, y = afinn)) +
  geom_point()


#Sentiment Analysis grouping by title
corpus_sent_by_title = corpus(data_clean, text_field = "Lyric")
docid <- paste(data_clean$Title)
docnames(corpus_sent_by_title) <- docid
toks=tokens(corpus_sent_by_title)
toks_sent_by_title <- tokens_lookup(toks, dictionary = data_dictionary_AFINN)
dfsent_by_title <- dfm(toks_sent_by_title)
dfsent_by_title <- dfsent_by_title %>% dfm_remove(stopwords("en")) %>% dfm_remove(words_to_remove)
dict_output_by_title <- convert(dfsent_by_title, to = "data.frame")
#Grouping by title
sum_by_title <- dict_output_by_title[order(dict_output_by_title$afinn, decreasing = TRUE),]
#Plot of 10 best scores by song
ggplot(data = head(sum_by_title,10), aes(x = doc_id, y = afinn)) + ylim(0,250)+
  geom_point()
#Plot of 10 best scores by song
ggplot(data = head(sum_by_title,30), aes(x = doc_id, y = afinn)) + ylim(0,250)+
  geom_point()
