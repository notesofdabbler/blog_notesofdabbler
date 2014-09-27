
# Scraping twitter data - Tweets with a particular hashtag

# Heavily borrowed/adapted codes from several web sources and more notably the one below
# http://davetang.org/muse/2013/04/06/using-the-r_twitter-package/
# http://www.rdatamining.com/examples/text-mining
# http://thinktostart.com/create-twitter-sentiment-word-cloud-in-r/
# http://heuristically.wordpress.com/2011/04/08/text-data-mining-twitter-r/

# load libraries
library(twitteR)
library(tm)
library(wordcloud)
library(dplyr)
library(topicmodels)
library(RColorBrewer)
library(igraph)

# set working directory
setwd("~/notesofdabbler/githubfolder/blog_notesofdabbler/exploreTwitter/")

#necessary step for Windows
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

source("keys.R")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
cred <- OAuthFactory$new(consumerKey=myKey,
                         consumerSecret=mySecret,
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

#necessary step for Windows
cred$handshake()
#save for later use for Windows
save(cred, file="twitterAuthentication.Rdata")
registerTwitterOAuth(cred)


getTw <- searchTwitter("#Mangalyaan",n=1500,since="2014-09-26",until="2014-09-27",cainfo="cacert.pem")

getTw_txt <- sapply(getTw, function(x) x$getText())
getTw_rt <- sapply(getTw,function(x) x$getIsRetweet())
getTw_dt <- do.call(c,lapply(getTw,function(x) x$created))
getTw_dt <- as.Date(getTw_dt,format="%Y%m%d")
getTw_df <- data.frame(date=getTw_dt,txt=getTw_txt,rt=getTw_rt)
table(getTw_df$date)

save(getTw_df,file="getTw_df_20140926.Rda")
