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

tokens = tokens_select(tokens, stopwords("english"),
                       selection = "remove")

tokens = tokens_select(tokens, c("lol", "rt"), selection = "remove", padding = FALSE)

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

### Create a Corpora and a N-gram to this text
### Create a Wordcloud. 
### How to create a model that analysis everyday the same information. 
### Proximity tokens of each candidate


