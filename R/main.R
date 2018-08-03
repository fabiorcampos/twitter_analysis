### Libraries
library(twitteR)
library(ggplot2)
library(quanteda)
library(dplyr)

### Twitter Connection
consumer_key = 'YetGrrFzoeN0UUQfCZoQERsYg'
consumer_secret = 'xb2sXMwcLOP5HNERJBgPw2gCHUPjSSFy0VLhRScgJjfkCaP79K'
access_token = '293631173-p2Kv4K7BW41D0vNXc5OQgdBR5nCwR4D7pmiiTgyh'
access_secret = 'JpO4Wik99qyREddOfi0zdnxhsUdhhv1PhjbL52i4gqoHv'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

### Search on Twitter
tweets = searchTwitter("presidente",n=3000,lang="pt",resultType = "recent")

### Convert Data Frame
tweets_df = twListToDF(tweets)
df = tweets_df[,c(1,5,12,13,14)]

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


### Delete Retweet texts


### Clean and organize data
tokens = df$text 
tokens = tokens(tokens, what = "word",
                remove_numbers = TRUE, remove_punct = TRUE,
                remove_symbols = TRUE, remove_hyphens = TRUE,
                remove_twitter = TRUE, remove_url = TRUE)