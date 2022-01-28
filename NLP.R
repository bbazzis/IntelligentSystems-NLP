library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(quanteda)
library("quanteda.sentiment")
library(tidyr)
library("quanteda.textplots")
library("quanteda.textstats")

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
#corpus <- corpus(unlist(data_clean))
corpus
dfm1 <- dfm(tokens(corpus))
topfeatures(dfm1)

dfm2 <- dfm_remove(dfm1, stopwords("en"))
words_to_remove <- c("rihanna","ya", "t", "hey", "eh", "da", "oh","ya","pre","da","la","uh" )
dfm2 <- dfm_remove(dfm2, words_to_remove)
topfeatures(dfm2, n=100)

dfm_trim1<-dfm_trim(dfm2, min_termfreq = 10, verbose = FALSE)
set.seed(100)
textplot_wordcloud(dfm_trim1)
textplot_wordcloud(dfm_trim1, max_words = 75)
################
library("quanteda.textstats")
features_dfm_inaug <- textstat_frequency(dfm2, n = 100)
features_dfm_inaug$feature <- with(features_dfm_inaug, reorder(feature, -frequency))
ggplot(features_dfm_inaug, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
##################
album = unique(data_clean$Album)
album
dfm3 <- unlist(data_clean$Album) %>% corpus() %>% tokens() %>% 
  tokens_remove(stopwords("english")) %>%
  tokens_remove(words_to_remove) %>%
  dfm() %>% dfm_group(groups = album) %>%
  dfm_weight(scheme= "prop")
####
data_w_title =  data_clean
data_w_title$Title <- NULL
corpus2 = corpus(unlist(data_w_title))
corpus2 <- corpus_subset(corpus2, "Album" %in% c(unique(data_w_title$Album))) 
corpus2
album = data_clean$Album
dfm3 <- corpus2 %>% tokens() %>% 
  tokens_remove(stopwords("english")) %>%
  tokens_remove(words_to_remove) %>%
  dfm() %>% dfm_group(groups = "Album") %>%
  dfm_weight(scheme= "prop")
####
df_4 <- unlist(data_clean) %>% corpus() %>% tokens()
  
freq_weight <- textstat_frequency(dfm2,
                                  groups = dfm2$Album)
ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency")


##################################



#####################
afinn <- read.delim(system.file("extdata/afinn/AFINN-111.txt", 
                                package = "quanteda.sentiment"),
                    header = FALSE, col.names = c("word", "valence"))
head(afinn)
data_dictionary_afinn <- dictionary(list(afinn = afinn$word))
#valence(data_dictionary_afinn) <- list(afinn = afinn$valence)
#toks = tokens(corpus(data_clean$Lyric))
#tokssel <- tokens_select(toks, data_dictionary_afinn)
#tokssel
#valence(data_dictionary_afinn)$afinn[as.character(tokssel)]
corpus365 = corpus(data_w_title, text_field = "Lyric")
docid <- paste(data_w_title$Album)
docnames(corpus365) <- docid
corpus365
toks=tokens(corpus365)
toks_gov_lsd <- tokens_lookup(toks, dictionary = data_dictionary_afinn)
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

