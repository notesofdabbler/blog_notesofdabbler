
#
# Analyze trip advisor data
#

# set working directory
setwd("~/notesofdabbler/blog_notesofdabbler/hotelReview/")

library(dplyr)
library(lubridate)
library(ggplot2)
library(tm)
library(scales)
library(topicmodels)

# load data for a hotel
# currently 3 datasets available from scraping trip advisor data
#    1. J W Marriott, Indianapolis (label: jwmarriott)
#    2. Hampton Inn Indianapolis Northwest -100 (label: hamptoninn)
#    3. Conrad Indianapolis (label: conrad)
#

pickhotel="conrad"
filenm=paste("dfrating_",pickhotel,".Rda",sep="")
load(filenm)
head(dfrating)
# number of reviews in data
summary(dfrating)
# frequency of star rating
ratingFreq=table(dfrating$ratingnum)
ratingFreq

# create a month label
dfrating$yrmon=floor_date(dfrating$ratingdt,"month")

# get rating range by month
dfrating.bymon=dfrating%>%group_by(yrmon,ratingnum)%>%summarize(count=n())
dfrating.bymon.agg=dfrating.bymon%>%group_by(yrmon)%>%summarize(countfull=sum(count))
dfrating.bymon=merge(dfrating.bymon,dfrating.bymon.agg,c("yrmon"))
dfrating.bymon$pctrating=dfrating.bymon$count/dfrating.bymon$countfull

p=ggplot(dfrating.bymon,aes(x=yrmon,y=pctrating,color=factor(ratingnum)))+geom_line(size=1.2)
p=p+xlab("")+ylab("% of Ratings")
p=p+scale_y_continuous(breaks=seq(0,1,0.1),labels=percent)+scale_color_discrete(name="# stars")
p=p+theme_minimal()
p


# Explore top level quotes for each rating


getDTM=function(dftxt){

  # code adapted from http://www.rdatamining.com/examples/text-mining
  
  txtcorpus=Corpus(VectorSource(dftxt))
  inspect(txtcorpus[1:5])
  
  txtcorpus.cl=tm_map(txtcorpus,tolower)
  txtcorpus.cl=tm_map(txtcorpus.cl,removePunctuation)
  txtcorpus.cl=tm_map(txtcorpus.cl,removeNumbers)
  
  mystopwords=c(stopwords("english"),"hotel","staff","room","rooms","indianapolis","marriott","conference",
                "convention","indy","downtown","hampton","stay","stayed","inn","conrad")
  txtcorpus.cl=tm_map(txtcorpus.cl,removeWords,mystopwords)
  #dictCorpus=txtcorpus.cl
  #txtcorpus.cl=tm_map(txtcorpus.cl,stemDocument)
  #txtcorpus.cl=tm_map(txtcorpus.cl,stemCompletion,dictionary=dictCorpus)
  
  dtm=DocumentTermMatrix(txtcorpus.cl)
  
  dtm.m=as.matrix(dtm)
  
  return(dtm.m)
}

minrating=min(dfrating$ratingnum)
maxrating=max(dfrating$ratingnum)
tfreq.l=as.list(rep(NA,maxrating-minrating+1))

# of frequent words to retain
numterms=20

rating=maxrating+1

for(i in 1:(maxrating-minrating+1)){
  
  rating=rating-1
  sprintf("Processing data for %s stars",rating)
  
  dftxt=dfrating$topquote[dfrating$ratingnum==rating]

  dtm.m=getDTM(dftxt)
  
  tfreq=colSums(dtm.m)
  tfreq.l[[i]]=names(sort(tfreq,decreasing=TRUE)[1:numterms])
}

topTerms=do.call(cbind,tfreq.l)
colnames(topTerms)=paste(names(ratingFreq),"star",sep=" ")[(maxrating-minrating+1):1]
topTerms

# Further investigation of high star rating
dftxt=dfrating$fullrev[dfrating$ratingnum>=4]
dtm.m=getDTM(dftxt)

# clustering of words to detect themes/topics

set.seed(1234)
txtclust=kmeans(t(dtm.m),5)

# size of clusters
txtclust$size

# within and total sum of squares
txtclust$totss
txtclust$withinss

# get list of frequent terms in each cluster
clustTerms=as.list(rep(NA,5))

termlist=colnames(dtm.m)
for(i in 1:5){
  termlist.filt=termlist[txtclust$cluster == i]
  tfreq=colSums(dtm.m)
  tfreq.filt=sort(tfreq[termlist.filt],decreasing=TRUE)
  clustTerms[[i]]=names(tfreq.filt[1:20])  
}
clust.topic=do.call(cbind,clustTerms)
clust.topic[is.na(clust.topic)]=""
colnames(clust.topic)=c("cluster 1","cluster 2","cluster 3","cluster 4","cluster 5")

# print list of frequent terms in each cluster
clust.topic

