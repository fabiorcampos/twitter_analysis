### Libraries
library(twitteR)
library(ggplot2)
library(quanteda)
library(dplyr)
library(stringr)

### Twitter Connection
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

### Search on Twitter
terms = c("bolsonaro","alckmin", "Boulos")
tweets = searchTwitter("bolsonaro",n=3000,lang="pt",resultType = "recent")

### Convert Data Frame
tweets_df = twListToDF(tweets)
df = tweets_df[,c(1,5,11,12,13,14)]

### Create a text length variable
df$textlength = nchar(df$text)

### Create Id Column
idvector = c(1:3000)
df = cbind(df, idvector)

### How Many tweets are retweeted? 
barplot_retweet = barplot(table(df$isRetweet), main = "Retweeted", ylab = "Quantity", ylim = c(0, 2500),
                          names.arg = c("NO", "YES"), col = c("RED", "BLUE"))

table(df$isRetweet)

### Top 10 Retweets
top10 = top_n(df, 10, retweetCount)
top10

### Remove duplicated rows that is retweet count
table(duplicated(df$text))
df = df[!duplicated(df$text),]

### Create Tokens and Clean data
tokens = df$text 
tokens = tokens(tokens, what = "word",
                remove_numbers = TRUE, remove_punct = TRUE,
                remove_symbols = TRUE, remove_hyphens = TRUE,
                remove_twitter = TRUE, remove_url = TRUE)

tokens = tokens_select(tokens, stopwords("pt"),
                       selection = "remove")

tokens = tokens_select(tokens, c("lol", "rt"), selection = "remove", padding = FALSE)

stop_words = readLines("./data/stopwords_ptbr.txt")

tokens = tokens_select(tokens, stop_words, selection = "remove", padding = FALSE)

### Insert news columns with Tokens and clean texts "https://docs.quanteda.io/reference/as.tokens.html"
df$tokens = as.list(tokens)

### separate the retweeters @users. 
corpora = df$text
corpora = corpus(corpora)
corpora = corpus_segment(corpora, pattern = "@*:", extract_pattern = TRUE)
users = corpora[[1:5]]

### LeftJoin Table of retweeters @users
names_users = c("texts","_document","idvector","_segid","pattern")
names(users) = names_users
df = left_join(df, users, by = "idvector")
df = df[,-c(10:12)]

### Create a N-gram to this text
tokens_ngr = tokens_ngrams(tokens, n = 2:3)

### Plot a N-gram File

### Create a DFM
dfm = dfm(tokens_ngr, tolower = TRUE, stem = FALSE)

### Similarity and distance computation between documents or features
dfm_dist = textstat_dist(dfm, selection = NULL, margin = c("documents", "features"),
              method = "euclidean", upper = FALSE, diag = FALSE, p = 2)

dfm_simil = textstat_simil(dfm, selection = NULL, margin = c("documents", "features"),
               method = "correlation", upper = FALSE, diag = FALSE)

### Plot a Word Frequencies in DFM
topfeatures(dfm, 50)

### Identify and score multi-word expressions, or adjacent fixed-length collocations, from text.
collocation = textstat_collocations(tokens, size = 5, tolower = FALSE)
head(collocation, 10)

### How to create a model that analysis everyday the same information.
### Remove patterns tokens that is useful
### Create dictionaries

### Construct a Textplot Network
dfm_= dfm(tokens, tolower = TRUE, stem = FALSE, ngrams = 1, remove = c(stop_words, "pt"))
fcm = fcm(dfm_)
feat = names(topfeatures(fcm, 50))
fcm = fcm_select(fcm, feat)

size = log(colSums(dfm_select(dfm_, feat)))
textplot_network(fcm, min_freq = 0.8, vertex_size = size / max(size) * 3)

### Create a Wordcloud
textplot_wordcloud(dfm_, max_words = 30)

### Locate Keywords-in-context for each candidate
kwic = kwic(tokens, "bolsonaro", window = 3)
head(kwic, n = 20)

### Textstat frequencies of dfm and dfm_
text_dfm_ngram = textstat_frequency(dfm, n = 30)
text_dfm_ = textstat_frequency(dfm_, n = 30)

ggplot(text_dfm_ngram[1:20, ], aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency")

ggplot(text_dfm_[1:20, ], aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency")

