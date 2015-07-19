#
# Clean affiliation list
#

# load libraries
library(dplyr)
library(tm)

# author and affiliation data
load(file = "xtauthdf.Rda")
# author data
load(file = "authdf.Rda")

xtauthdf2 = xtauthdf[,c("id","xtauthor2")]
names(xtauthdf2) = c("id","author2")

authdf2 = authdf[,c("id","author2")]

affil = setdiff(xtauthdf2,authdf2)

affillist = unique(affil$author2)

corpus = VCorpus(VectorSource(affillist))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
dtm <- DocumentTermMatrix(corpus)
