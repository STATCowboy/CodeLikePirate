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
N=2000  # tweets to request from each query

S=200  # radius in miles
lats=c(38.9,40.7,37.8,39,37.4,28,30,42.4,48,36,32.3,33.5,34.7,33.8,37.2,41.2,46.8,
       46.6,37.2,43,42.7,40.8,36.2,38.6,35.8,40.3,43.6,40.8,44.9,44.9)

lons=c(-77,-74,-122,-105.5,-122,-82.5,-98,-71,-122,-115,-86.3,-112,-92.3,-84.4,-93.3,
       -104.8,-100.8,-112, -93.3,-89,-84.5,-111.8,-86.8,-92.2,-78.6,-76.8,-116.2,-98.7,-123,-93)

# list of cities that coordinate to the lat/lons
#cities=DC,New York,San Fransisco,Colorado,Mountainview,Tampa,Austin,Boston,
#       Seatle,Vegas,Montgomery,Phoenix,Little Rock,Atlanta,Springfield,
#       Cheyenne,Bisruk,Helena,Springfield,Madison,Lansing,Salt Lake City,Nashville
#       Jefferson City,Raleigh,Harrisburg,Boise,Lincoln,Salem,St. Paul

# Actually Seach Twitter
donald=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('Donald+Trump',
                                                                      lang="en",n=N,resultType="recent",
                                                                      geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","), retryOnRateLimit = 2000)))


# Functions to cleanup data from Twitter
donaldlat=sapply(donald, function(x) as.numeric(x$getLatitude()))
donaldlat=sapply(donaldlat, function(z) ifelse(length(z)==0,NA,z))  

donaldlon=sapply(donald, function(x) as.numeric(x$getLongitude()))
donaldlon=sapply(donaldlon, function(z) ifelse(length(z)==0,NA,z))  

donalddate=lapply(donald, function(x) x$getCreated())
donalddate=sapply(donalddate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

donaldtext=sapply(donald, function(x) x$getText())
donaldtext=unlist(donaldtext)

isretweet=sapply(donald, function(x) x$getIsRetweet())
retweeted=sapply(donald, function(x) x$getRetweeted())
retweetcount=sapply(donald, function(x) x$getRetweetCount())

favoritecount=sapply(donald, function(x) x$getFavoriteCount())
favorited=sapply(donald, function(x) x$getFavorited())

# Build a Data Frame of the Twitter Data
data=as.data.frame(cbind(tweet=donaldtext,date=donalddate,lat=donaldlat,lon=donaldlon,
                         isretweet=isretweet,retweeted=retweeted, retweetcount=retweetcount,favoritecount=favoritecount,favorited=favorited))

# This is code is to save and load the results for later if needed
#
# save(data, file="DonalTrumpData_20161121.Rdata")
# load(file="DonalTrumpData_20161121.Rdata")


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

# Use Lat and Lon to reverse geocode to get address
# Commented out as most tweets don't have the lat/long 
# (people turn of location services these days!)

# data=filter(data, !is.na(lat),!is.na(lon))
# lonlat=select(data,lon,lat)
# 
# result <- do.call(rbind, lapply(1:nrow(lonlat),
#                                 function(i) revgeocode(as.numeric(lonlat[i,1:2]))))
# 
# 
# data2=lapply(result,  function(x) unlist(strsplit(x,",")))
# address=sapply(data2,function(x) paste(x[1:3],collapse=''))
# city=sapply(data2,function(x) x[2])
# stzip=sapply(data2,function(x) x[3])
# zipcode = as.numeric(str_extract(stzip,"[0-9]{5}"))   
# state=str_extract(stzip,"[:alpha:]{2}")
# data2=as.data.frame(list(address=address,city=city,zipcode=zipcode,state=state))

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
hist(score,xlab=" ",main="Sentiment of sample tweets that have Donald Trump in them ",
     border="black",col="skyblue")

# Write data out to CSV
# write.csv(data, file="DonalTrumpData_20161121.csv")




