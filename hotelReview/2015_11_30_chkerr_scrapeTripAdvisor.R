#-------------------------------------------------------------------------------------------------------------------------
#  IMPORTANT NOTE:
#  This code for scraping was written before I had knowledge of the package rvest (https://github.com/hadley/rvest)
#  My code here is not robust. I have had instances where some change in underlying html has caused this code to break
#  rvest has a very clean syntax and is easy to use
#  rvest has an example on scraping this type of data (https://github.com/hadley/rvest/blob/master/demo/tripadvisor.R)
#  that is more robust and the code is much cleaner
#  At some point, I will refactor this code to use rvest
#-------------------------------------------------------------------------------------------------------------------------

#
# Bug reported at 
# https://notesofdabbler.wordpress.com/2014/08/15/exploring-hotel-review-data-from-trip-advisor-with-r/
# Looking into a bug report on the code
#

# set working directory
setwd("~/notesofdabbler/githubfolder/blog_notesofdabbler/hotelReview/")

# load libraries
library(RCurl)
library(XML)
library(lubridate)
library(ggplot2)

options(stringsAsFactors=FALSE)

#
# Web page is parsed based on url
# Search for a hotel gives results spanning several pages
# the function below extracts information for a given page
#

getOnePage=function(urllink){

  # get html page content
  doc=htmlTreeParse(urllink,useInternalNodes=TRUE)
  
  ## get node sets
  # review id
  ns_id=getNodeSet(doc,"//div[@class='quote isNew' or @class='quote ' or @class='quote']/a[@href]") 
  # top quote for a review
  ns_topquote=getNodeSet(doc,"//div[@class='quote isNew' or @class='quote ' or @class='quote']/a[@href]/span") 
  # get partial entry for review that shows in the page
  ns_partialentry=getNodeSet(doc,"//div[@class='col2of2']//p[@class='partial_entry'][1]")
  # date of rating
  ns_ratingdt=getNodeSet(doc,"//div[@class='col2of2']//span[@class='ratingDate relativeDate' or @class='ratingDate']")
  # rating (number of stars)
  ns_rating=getNodeSet(doc,"//div[@class='col2of2']//span[@class='rate sprite-rating_s rating_s']/img[@alt]")
  
  # get actual values extracted from node sets
  # review id
  id=sapply(ns_id,function(x) xmlAttrs(x)["id"])
  # top quote for the review
  topquote=sapply(ns_topquote,function(x) xmlValue(x))
  # rating date (couple of formats seem to be used and hence a and b below)
  ratingdta=sapply(ns_ratingdt,function(x) xmlAttrs(x)["title"])
  ratingdtb=sapply(ns_ratingdt,function(x) xmlValue(x))
  # rating (number of stars)
  rating=sapply(ns_rating,function(x) xmlAttrs(x)["alt"])
  # partial entry for review
  partialentry=sapply(ns_partialentry,function(x) xmlValue(x))
  
  # get rating date in date format
  ratingdt.pick=ratingdta
  ratingdt.pick[is.na(ratingdta)]=ratingdtb[is.na(ratingdta)]
  ratingdt=mdy(gsub("Reviewed ","",ratingdt.pick))
  
  # put all the fields in a dataframe
  dfrating=data.frame(id=id,topquote=topquote,ratingdt=ratingdt,rating=rating,partialentry=partialentry)
  dfrating$ratingnum=as.numeric(substr(dfrating$rating,1,1),1,1)
  
  return(dfrating)
  
}

#--- This part is manual and based on looking up the search url after typing search term in tripadvisor site------
# main url link for different hotels
# 3 hotels considered here
#    1. J W Marriott, Indianapolis
#    2. Hampton Inn Indianapolis Northwest -100
#    3. Conrad Indianapolis
#

##--------Changed urlmainlist to url that gave error----------------
# main url for the hotel search
urlmainlist=c(
  museum = "http://www.tripadvisor.com/Attraction_Review-g186605-d216337-Reviews-National_Museum_of_Ireland_Decorative_Arts_History-Dublin_County_Dublin.html"
  )

# counter used along with main url for additional search pages for a hotel
morepglist=list(
  museum=seq(10,650,10)
  )
#----------------------------------------------------------------------------------------------------------

# pick hotel for which review data is to be extracted
# choices: jwmarriott,hamptoninn,conrad
pickhotel="museum"


# get list of urllinks corresponding to different pages

# url link for first search page
urllinkmain=urlmainlist[pickhotel]
# counter for additional pages
morepg=as.numeric(morepglist[[pickhotel]])

urllinkpre=paste(strsplit(urllinkmain,"Reviews-")[[1]][1],"Reviews",sep="")
urllinkpost=strsplit(urllinkmain,"Reviews-")[[1]][2]

urllink=rep(NA,length(morepg)+1)

urllink[1]=urllinkmain
for(i in 1:length(morepg)){
  urllink[i+1]=paste(urllinkpre,"-or",morepg[i],"-",urllinkpost,sep="")
}
head(urllink)

# get summary content with rating

# note: there are few format that are not captured in scraping
# that is reflected in some reviews for Conrad not being extracted

dfrating.l=as.list(rep(NA,length(morepg)+1))

for(i in 1:(length(morepg)+1)){
  dfrating.l[[i]]=getOnePage(urllink[i])
  print(i)
}
dfrating=do.call(rbind,dfrating.l)
head(dfrating)

# get full reviews

# get url list that contain full reviews
## --- Original code involved substituting hotel review in url but the museum url has attraction_review in the url
## that needs to be substituted
urllinkpre.rev=gsub("Attraction_Review","ShowUserReviews",strsplit(urllinkmain,"Reviews-")[[1]][1])
urlrev=paste(urllinkpre.rev,"rXX-",urllinkpost,sep="")

dfrating$id2=gsub("rn","",dfrating$id)
urlfullrevlist=sapply(dfrating$id2,function(x) gsub("XX",x,urlrev))
head(urlfullrevlist)

# function to extract full review given review id and full review urllink
getfullrev=function(urllink,id){
  
  # get html content of page containing full review
  docrev=htmlTreeParse(urllink,useInternalNodes=TRUE)
  # extract node set containing full review
  revid=paste("review_",id,sep="")
  qry=paste("//p[@id='",revid,"']",sep="")
  ns_fullrev=getNodeSet(docrev,eval(qry))
  # get full review content
  return(xmlValue(ns_fullrev[[1]]))  
  
}

# Get the full review content
fullrev=rep(NA,nrow(dfrating))
for(i in 1:nrow(dfrating)){
  fullrev[i]=getfullrev(urlfullrevlist[i],dfrating$id2[i])
  if(i %% 20 == 0) print(i)
}
head(fullrev)

dfrating$fullrev=fullrev

# save to Rdataset
filenm=paste("2015_11_30_dfrating_",pickhotel,".Rda",sep="")
save(dfrating,file=filenm)
