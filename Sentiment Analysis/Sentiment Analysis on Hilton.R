# 
# Jamey Johnston
# 2016-11-21
# Adapted from: Sentiment Analysis on Donald Trump using R and Tableau 
# https://www.r-bloggers.com/sentiment-analysis-on-donald-trump-using-r-and-tableau/
# 

library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(stringi)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(wordcloud)

# Cleanup objects
rm(list=ls())

# Secret keys to the Twitter Dev Kingdom
key="<Enter YOUR KEY Here>"
secret="<Enter YOUR KEY Here>"
accessToken="<Enter YOUR KEY Here>"
accessTokenSecret="<Enter YOUR KEY Here>"

# Set the Working Directory (needed to output files and read files)
setwd("C:/Users/Jamey/OneDrive/Documents/Statistics/R/Sentiment Analysis")

# Access Twitter
download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="cacert.pem",
              method="auto")
authenticate <- OAuthFactory$new(consumerKey=key,
                                 consumerSecret=secret,
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="https://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")
setup_twitter_oauth(key, secret, accessToken, accessTokenSecret)
save(authenticate, file="twitter authentication.Rdata")

# Set the number of tweets to pull from each requests
N=20000  # tweets to request from each query

# Actually Seach Twitter
hilton=searchTwitter('hilton+hotels', lang="en",n=N,since='2016-09-01', retryOnRateLimit = 2000)

# Functions to cleanup data from Twitter
hiltondate=lapply(hilton, function(x) x$getCreated())
hiltondate=sapply(hiltondate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

hiltontext=sapply(hilton, function(x) x$getText())
hiltontext=unlist(hiltontext)

isretweet=sapply(hilton, function(x) x$getIsRetweet())
retweeted=sapply(hilton, function(x) x$getRetweeted())
retweetcount=sapply(hilton, function(x) x$getRetweetCount())

favoritecount=sapply(hilton, function(x) x$getFavoriteCount())
favorited=sapply(hilton, function(x) x$getFavorited())

# Build a Data Frame of the Twitter Data
data=as.data.frame(cbind(tweet=hiltontext,date=hiltondate, isretweet=isretweet,
                         retweeted=retweeted, retweetcount=retweetcount,favoritecount=favoritecount,favorited=favorited))

# This is code is to save and load the results for later if needed
#
# save(data, file="HiltonData_20161121.Rdata")
# load(file="HiltonData_20161121.Rdata")


# Create corpus
corpus=Corpus(VectorSource(data$tweet))

# Remove non-convertible bytes in corpus
corpus=tm_map(corpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))

# Remove Punctuation
corpus=tm_map(corpus, content_transformer(removePunctuation))

# Convert to lower-case
corpus=tm_map(corpus, content_transformer(stri_trans_tolower))

# Remove http
corpus=tm_map(corpus, content_transformer(function(x) gsub("htt.*",' ',x)))

# Remove stopwords
corpus=tm_map(corpus, content_transformer(function(x) removeWords(x,stopwords())))
# Remove Custom stopwords
# corpus=tm_map(corpus, content_transformer(function(x) removeWords(x,c("jamey","johnston","EnterMoreWords"))))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus, PlainTextDocument)

# Build WordCloud
col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=25, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)

# Sentiment Analysis
#

# Cleanup the data some (remove wierd chars and http)
tweet=data$tweet
tweet_list=lapply(tweet, function(x) iconv(x, "latin1", "ASCII", sub=""))
tweet_list=lapply(tweet, function(x) gsub("htt.*",' ',x))
tweet=unlist(tweet)
data$tweet=tweet

# Word lists for Sentiment Analysis
positives= readLines("positive-words.txt")
negatives = readLines("negative-words.txt")

# Function(s) to actually do sentiment analysis
sentiment_scores = function(tweets, positive_words, negative_words, .progress='none'){
  scores = laply(tweets,
                 function(tweet, positive_words, negative_words){
                   tweet = gsub("[[:punct:]]", "", tweet)    # remove punctuation
                   tweet = gsub("[[:cntrl:]]", "", tweet)   # remove control characters
                   tweet = gsub('\\d+', '', tweet)          # remove digits
                   
                   # Let's have error handling function when trying tolower
                   tryTolower = function(x){
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply
                   tweet = sapply(tweet, tryTolower)
                   # split sentence into words with str_split function from stringr package
                   word_list = str_split(tweet, "\\s+")
                   words = unlist(word_list)
                   # compare words to the dictionaries of positive & negative terms
                   positive_matches = match(words, positive_words)
                   negative_matches = match(words, negative_words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   positive_matches = !is.na(positive_matches)
                   negative_matches = !is.na(negative_matches)
                   # final score
                   score = sum(positive_matches) - sum(negative_matches)
                   return(score)
                 }, positive_words, negative_words, .progress=.progress )
  return(scores)
}

# Call Functions to do sentiment analysis and get scores
score = sentiment_scores(tweet, positives, negatives, .progress='text')
data$score=score

# Show Histogram of Scores
hist(score,xlab=" ",main="Sentiment of sample tweets that have Hilton Hotels in them ",
     border="black",col="skyblue")





