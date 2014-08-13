
#
#  Test scraping trip advisor
#

# set working directory
setwd("~/notesofdabbler/Rspace/hotelReview/")

# load libraries
library(RCurl)
library(XML)
library(lubridate)
library(ggplot2)

options(stringsAsFactors=FALSE)

urllink="http://www.tripadvisor.com/Hotel_Review-g37209-d88168-Reviews-Hyatt_Regency_Indianapolis-Indianapolis_Indiana.html"

doc=htmlTreeParse(urllink,useInternalNodes=TRUE)

ns_id=getNodeSet(doc,"//div[@class='quote']/a[@href]")
ns_topquote=getNodeSet(doc,"//div[@class='quote']/a[@href]/span")
ns_partialentry=getNodeSet(doc,"//div[@class='col2of2']//p[@class='partial_entry'][1]")
ns_ratingdt=getNodeSet(doc,"//div[@class='col2of2']//span[@class='ratingDate relativeDate' or @class='ratingDate']")
ns_rating=getNodeSet(doc,"//div[@class='col2of2']//span[@class='rate sprite-rating_s rating_s']/img[@alt]")


id=sapply(ns_id,function(x) xmlAttrs(x)["id"])
topquote=sapply(ns_topquote,function(x) xmlValue(x))
ratingdta=sapply(ns_ratingdt,function(x) xmlAttrs(x)["title"])
ratingdtb=sapply(ns_ratingdt,function(x) xmlValue(x))
rating=sapply(ns_rating,function(x) xmlAttrs(x)["alt"])
partialentry=sapply(ns_partialentry,function(x) xmlValue(x))

ratingdt.pick=ratingdta
ratingdt.pick[is.na(ratingdta)]=ratingdtb[is.na(ratingdta)]
ratingdt=mdy(gsub("Reviewed ","",ratingdt.pick))

dfrating=data.frame(id=id,topquote=topquote,ratingdt=ratingdt,rating=rating,partialentry=partialentry)
dfrating$ratingnum=as.numeric(substr(dfrating$rating,1,1),1,1)

# get full reviews
dfrating$id2=gsub("rn","",dfrating$id)

# test for a single review

getfullrev=function(urllink,id){

  docrev=htmlTreeParse(urllink,useInternalNodes=TRUE)
  revid=paste("review_",id,sep="")
  qry=paste("//p[@id='",revid,"']",sep="")
  ns_fullrev=getNodeSet(docrev,eval(qry))
  return(xmlValue(ns_fullrev[[1]]))  
  
}


# test for a list of reviews
urlrev="http://www.tripadvisor.com/ShowUserReviews-g37209-d1762915-rXX-JW_Marriott_Indianapolis-Indianapolis_Indiana.html"
urlfullrevlist=sapply(dfrating$id2,function(x) gsub("XX",x,urlrev))

fullrev=rep(NA,5)
for(i in 1:5){
  fullrev[i]=getfullrev(urlfullrevlist[i],dfrating$id2[i])
}


