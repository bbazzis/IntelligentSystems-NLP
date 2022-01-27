library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(quanteda)
library(tidyr)
library("quanteda.textplots")

data <- read.csv("data/Rihanna.csv", na.strings = c("", "NA"))
data$X <- NULL
data$Album <- data$Album %>% replace_na("Single")
data$Lyric <- trimws(data$Lyric)



data_clean <- data %>% drop_na()
data_clean$Lyric <- trimws(data_clean$Lyric)

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
  df <- gsub("n'","ng ", df)
  df <- gsub("'cause","because", df)
  df <- gsub("y'","you ", df)
  df <- gsub("gonna","going to", df)
  df <- gsub("c amon","come on", df)
  return(df)
}
data_clean$Lyric <- sapply(data_clean$Lyric, modify_contractions)
specialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
data_clean$Lyric <- sapply(data_clean$Lyric, specialChars)

############
corpus <- corpus(data_clean$Lyric)
corpus
dfm1 <- dfm(tokens(corpus))
topfeatures(dfm1)

dfm2 <- dfm_remove(dfm1, stopwords("en"))
topfeatures(dfm2, n=30)

topfeatures(dfm1, n = 30,decreasing = FALSE)

dfm_trim1<-dfm_trim(dfm2, min_termfreq = 10, verbose = FALSE)
set.seed(100)
textplot_wordcloud(dfm_trim1)
################
library("quanteda.textstats")
features_dfm_inaug <- textstat_frequency(dfm2, n = 100)
features_dfm_inaug$feature <- with(features_dfm_inaug, reorder(feature, -frequency))
ggplot(features_dfm_inaug, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
##################

