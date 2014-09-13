#
#   Scrape AIChE 2014 technical program data
#

# set working directory
setwd("~/notesofdabbler/githubfolder/blog_notesofdabbler/exploreAICHE2014_techProgram/")

# load libraries
library(rvest)
library(magrittr)
library(dplyr)

urlpre <- "https://aiche.confex.com/aiche/2014/webprogram/author"
urllist <- paste(urlpre,letters,".html",sep="")

paperlinksTrunc=as.list(rep(NA,length(urllist)))
for(i in 1:length(urllist)){
  paperlinksTrunc[[i]] <- urllist[i]%>%html()%>%html_node(".papers a")%>%html_attr("href")
}

paperlinks=unlist(paperlinksTrunc)
paperlinks=unique(paperlinks)
head(paperlinks)



getpaperInfo <- function(paperurl){
  info=paperurl%>%html()
  title=info%>%html_node(".subtitle .subtext")%>%html_text()
  auth=info%>%html_node(".paperauthors")%>%html_text()
  session=info%>%html_node("//div[@class='parents']//a[@href][1]",xpath=TRUE)%>%html_text()
  topic=info%>%html_node("//div[@class='parents']//a[@href][2]",xpath=TRUE)%>%html_text()
  paperinfo=c(auth=auth,title=title,session=session,topic=topic)
  return(paperinfo)
}

paperurl=paste("https://aiche.confex.com/aiche/2014/webprogram/",paperlinks[i],sep="")
tmp=getpaperInfo(paperurl)

paperInfoList=as.list(rep(NA,length(paperlinks)))
for(i in 1:length(paperlinks)){
  paperurl=paste("https://aiche.confex.com/aiche/2014/webprogram/",paperlinks[i],sep="")
  paperInfoList[[i]]=getpaperInfo(paperurl)
  if(i %% 10 == 0) {print(i)}
}

paperInfo=do.call(rbind,paperInfoList)
dim(paperInfo)
paperInfo=data.frame(paperInfo)
head(paperInfo)

save(paperInfo,file="paperInfo.Rda")
