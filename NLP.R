library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(quanteda)
library("quanteda.sentiment")
library("quanteda.textplots")
library("quanteda.textstats")
library(tidyr)

data <- read.csv("data/Rihanna.csv", na.strings = c("", "NA"), encoding = "UTF-8")
data$X <- NULL
data$Artist <- NULL
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

############
corpus <- corpus(data_clean$Lyric)
dfm1 <- dfm(tokens(corpus))
topfeatures(dfm1)

words_to_remove <- c("rihanna","ya", "t", "hey", "eh", "da", "oh","ya","pre","da","la","uh" )
dfm2 <- dfm1 %>% dfm_remove(stopwords("en")) %>% dfm_remove(words_to_remove)
topfeatures(dfm2, n=100)

dfm_trim1<-dfm_trim(dfm2, min_termfreq = 10, verbose = FALSE)
set.seed(100)
textplot_wordcloud(dfm_trim1)
textplot_wordcloud(dfm_trim1, max_words = 75)

################
features_dfm_inaug <- textstat_frequency(dfm2, n = 100)
features_dfm_inaug$feature <- with(features_dfm_inaug, reorder(feature, -frequency))
ggplot(features_dfm_inaug, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
##################


##################################
corpus365 = corpus(data_clean, text_field = "Lyric")
toks=tokens(corpus365)
dfmat_gov_lsd <- dfm(toks)
dfmat_gov_lsd <- dfm_remove(dfmat_gov_lsd, stopwords("en"))
dfmat_gov_lsd <- dfm_remove(dfmat_gov_lsd, words_to_remove)
top_words_by_album<-topfeatures(dfmat_gov_lsd, groups = Album)
ggplot(top_words_by_album, aes(x = feature))
#####################
afinn <- read.delim(system.file("extdata/afinn/AFINN-111.txt", 
                                package = "quanteda.sentiment"),
                    header = FALSE, col.names = c("word", "valence"))
head(afinn)
data_dictionary_afinn <- dictionary(list(afinn = afinn$word))

corpus365 = corpus(data_clean, text_field = "Lyric")
docid <- paste(data_clean$Album)
docnames(corpus365) <- docid
corpus365
toks=tokens(corpus365)
toks_gov_lsd <- tokens_lookup(toks, dictionary = data_dictionary_AFINN)
print(toks_gov_lsd)
dfmat_gov_lsd <- dfm(toks_gov_lsd)
dfmat_gov_lsd <- dfm_remove(dfmat_gov_lsd, stopwords("en"))
dfmat_gov_lsd <- dfm_remove(dfmat_gov_lsd, words_to_remove)
dict_output <- convert(dfmat_gov_lsd, to = "data.frame")
dict_output$doc_id <- dfmat_gov_lsd@docvars[["Album"]]
sum <- data.frame(aggregate(afinn ~ doc_id, dict_output, sum))
sum <- sum[order(sum$afinn, decreasing = TRUE),]
ggplot(data = head(sum,15), aes(x = doc_id, y = afinn)) +
  geom_point()


#########################
corpus365 = corpus(data_clean, text_field = "Lyric")
corpus365
docid <- paste(data_clean$Title)
docid
docnames(corpus365) <- docid
corpus365
toks=tokens(corpus365)
toks_gov_lsd <- tokens_lookup(toks, dictionary = data_dictionary_AFINN)
dfmat_gov_lsd <- dfm(toks_gov_lsd)
print(dfmat_gov_lsd)
dfmat_gov_lsd <- dfm_remove(dfmat_gov_lsd, stopwords("en"))
dfmat_gov_lsd <- dfm_remove(dfmat_gov_lsd, words_to_remove)
dict_output <- convert(dfmat_gov_lsd, to = "data.frame")
sum <- dict_output[order(dict_output$afinn, decreasing = TRUE),]
ggplot(data = head(sum,15), aes(x = doc_id, y = afinn)) + ylim(0,250)+
  geom_point()
##############

