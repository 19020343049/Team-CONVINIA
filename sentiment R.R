getwd()
rm(list = ls())
install.packages('devtools')
install.packages('twitteR')
install.packages('ROAuth')
install.packages('plyr')
install.packages('dplyr')
install.packages('stringr')
install.packages('ggplot2')
install.packages('httr')
install.packages('wordcloud')
install.packages('tm')
install.packages('RCurl')
install.packages('syuzhet')



library('devtools')
library('twitteR')
library('ROAuth')
library('plyr')
library('dplyr')
library('stringr')
library('ggplot2')
library('httr')
library('wordcloud')
library('tm')
library('RCurl')
library('syuzhet')


consumerKey="AgnlboLoCE8D8mGD306NV52HP"
#Replace with your consumerKey
consumerSecret="zTGF55AeviuW7qiqHD8RWdNXkekknhKiJXTtnlfpLlRtX9qF2O"
#Replace with your consumerSecret
accesstoken="1266975421851107328-juQcFThlBlyra6qd3A4AMqqTrXDibH"
#Replace with your accesstoken
accesssecret="LhRKi4Cna87r4ttrzwCEjhWB1sFLMKGA2th9UwA8gwnHo"
#Replace with your accesssecret



setup_twitter_oauth(consumerKey, consumerSecret, accesstoken, accesssecret)
some_tweets = searchTwitter("corona+ covid-19+ COVID+ coronavirus", n=2000, lang= "en")

View(some_tweets)
some_tweets.df <- ldply(some_tweets, function(t) t$toDataFrame())
View(some_tweets.df)




some_txt = sapply(some_tweets, function(x) x$getText())
some_txt



some_txt1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",some_txt)
some_txt1
some_txt2 = gsub("http[^[:blank:]]+", "", some_txt1)
some_txt2

some_txt3 = gsub("@\\w+", "", some_txt2)
some_txt3

some_txt5 = gsub("[^[:alnum:]]", " ", some_txt3)
some_txt5
some_txt6 <- Corpus(VectorSource(some_txt5))
some_txt6

some_txt6 <- tm_map(some_txt6, removePunctuation)
some_txt6 <- tm_map(some_txt6, content_transformer(tolower))
some_txt6 <- tm_map(some_txt6, removeWords, stopwords("english"))
some_txt6 <- tm_map(some_txt6, stripWhitespace)

#sentiment 

mysentiment <- get_nrc_sentiment(some_txt5)

SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL


ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")


