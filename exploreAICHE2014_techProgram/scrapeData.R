#
#   Scrape AIChE 2014 technical program data
#

# set working directory
setwd("~/notesofdabbler/githubfolder/blog_notesofdabbler/exploreAICHE2014_techProgram/")

# load libraries
library(rvest)
library(magrittr)
library(dplyr)
library(stringr)

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

load(file="paperInfo.Rda")
tmp=data.frame(lapply(paperInfo,as.character),stringsAsFactors=FALSE)
tmp$auth=gsub("[^a-zA-Z, ]+","",tmp$auth)
paperInfo=tmp
paperInfo$id=seq(1,nrow(paperInfo))

save(paperInfo,file="paperInfo.Rda")

authList=list()
affilList=list()
for(i in 1:nrow(paperInfo)){
  authsplit=str_split(paperInfo$auth[i],"and ",n=2)
  if(length(authsplit[[1]])>1){
    authonly=unlist(c(str_split(authsplit[[1]][1],", "),str_split(authsplit[[1]][2],", ")[[1]][1]))
    affilonly=str_split(authsplit[[1]][2],", ")[[1]][-1]
  } else {
    authonly=unlist(str_split(authsplit[[1]][1],", ")[[1]][1])
    affilonly=str_split(authsplit[[1]][1],", ")[[1]][-1]
  }
  authList[[i]]=data.frame(id=i,authonly,stringsAsFactors=FALSE)
  affilList[[i]]=data.frame(id=i,affilonly,stringsAsFactors=FALSE) 
}

authdf=rbind_all(authList)
affildf=rbind_all(affilList)

save(authdf,file="authdf.Rda")
save(affildf,file="affildf.Rda")
