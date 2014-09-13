#
#  Analyze AIChE 2014 tech program data
#

# set working directory
setwd("~/notesofdabbler/githubfolder/blog_notesofdabbler/exploreAICHE2014_techProgram/")

# load libraries
library(dplyr)
library(tm)

# load data
load(file="paperInfo.Rda")

sessions <- paperInfo%>%group_by(topic,session)%>%summarize(talks=n())
topics <- sessions%>%group_by(topic)%>%summarize(talks=sum(talks),sessions=n())

topics%>%arrange(desc(sessions))%>%select(topic,sessions)
topics%>%arrange(desc(talks))%>%select(topic,talks)

# select a topic of interest

pickTopic <- "Pharmaceutical Discovery, Development and Manufacturing Forum"
topicDetail <- paperInfo%>%filter(topic==pickTopic)

# process titles
titles <- Corpus(VectorSource(topicDetail$title))
inspect(titles[1:5])

titles.cl <- tm_map(titles,tolower)
titles.cl <- tm_map(titles.cl,removePunctuation)
titles.cl <- tm_map(titles.cl,removeNumbers)

mystopwords=c(stopwords("english"))
titles.cl <- tm_map(titles.cl,removeWords,mystopwords)
titles.cl <- tm_map(titles.cl,PlainTextDocument)

myDTM <- DocumentTermMatrix(titles.cl)
myDTM.m <- as.matrix(myDTM)

tfreq=colSums(myDTM.m)
tfreq.df=data.frame(terms=names(tfreq),freq=tfreq)
tfreq.df=tfreq.df%>%arrange(desc(freq))

pca.fit=prcomp(myDTM.m)
plot(pca.fit)
summary(pca.fit)

load1=pca.fit$rotation[,3]
sort(abs(load1),decreasing=TRUE)[1:10]
