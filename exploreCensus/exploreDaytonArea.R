
# Explore Dayton area demographics and housing prices

# set working dir
setwd("~/notesofdabbler/githubfolder/blog_notesofdabbler/exploreCensus/")


# load libraries
library(XML)
library(RCurl)
library(stringr)
library(ggplot2)
library(maptools)
#library(rgeos)
library(ggmap)
library(RJSONIO)
library(RgoogleMaps)
library(dplyr)

# List of counties in Dayton area
# http://www.dayton-real-estate.com/Ohio%20County%20Map.html

dayton.counties=c("Shelby","Darke","Miami","Champaign","Clark",
                  "Preble","Montgomery","Greene","Butler","Warren")
cntylist=paste(dayton.counties," County, OH",sep="")

# load zip-conty data for Ohio
load(file="zipCityCountyStateMap.Rda")

# get list of zips in the Dayton area
zipLoc=zipMap2[zipMap2$ctyname %in% cntylist,]
# remove duplicates (due to a zip in multiple counties)
zipLoc2=zipLoc[!duplicated(zipLoc$ZCTA5),]
head(zipLoc2)

# get shape file of zips in Ohio
# http://www.census.gov/cgi-bin/geo/shapefiles2010/layers.cgi
zipShp=readShapePoly("tl_2010_39_zcta510/tl_2010_39_zcta510.shp")
zipShp.loc=zipShp[zipShp$ZCTA5CE10 %in% zipLoc2$ZCTA5,]


# get a dataframe from shape file
nzips=nrow(zipShp.loc@data)
shape.l=list()
k=0
for(i in 1:nzips){
  lpoly=length(zipShp.loc@polygons[[i]]@Polygons)
  for(j in 1:lpoly){
    if(zipShp.loc@polygons[[i]]@Polygons[[j]]@ringDir==1){
      k=k+1
      df=data.frame(zipShp.loc@polygons[[i]]@Polygons[[j]]@coords)
      names(df)=c("long","lat")
      df$order=seq(1,nrow(df))
      df$zip=zipShp.loc@data$ZCTA5CE10[i]
      df$id=paste(df$zip,"_",j,sep="")
      shape.l[[k]]=df
    }
  }
}
zipShp2=rbind_all(shape.l)

# filter to retail shapes for dayton area zips
zipShp3=zipShp2[zipShp2$zip %in% zipLoc2$ZCTA5,]

save(zipShp3,file="zipshpDayton.Rda")
#load(file="zipshpDayton.Rda")

# plot dayton area zips
x=get_googlemap(center="Dayton",maptype=c("roadmap"),zoom=9)
p=ggmap(x)
p=p+geom_polygon(data=zipShp3,aes(x=long,y=lat,group=id),fill="blue",color="black",alpha=0.2)
print(p)

# Get Census Data

# source keys, this file has the following code
#
# censusAPIkey="yourkey"
# zillowAPIkey="yourkey
#
source("keys.R")
APIkey=censusAPIkey

# state code (Ohio)
state=39

# function to retrieve data from 2010 US census data
getCensusData=function(APIkey,state,fieldnm){
  resURL=paste("http://api.census.gov/data/2010/sf1?get=",fieldnm,
               "&for=zip+code+tabulation+area:*&in=state:",state,"&key=",
               APIkey,sep="")
  dfJSON=fromJSON(resURL)
  dfJSON=dfJSON[2:length(dfJSON)]
  dfJSON_zip=sapply(dfJSON,function(x) x[3])
  dfJSON_val=sapply(dfJSON,function(x) x[1])
  df=data.frame(dfJSON_zip,as.numeric(dfJSON_val))
  names(df)=c("zip","val")
  return(df)
}

# Data from US census 2010 for a state
# get median age 
fieldnm="P0130001"
dfage=getCensusData(APIkey,state,fieldnm)
names(dfage)=c("zip","medAge")
head(dfage)

# get total population
fieldnm="P0100001"
dfpop=getCensusData(APIkey,state,fieldnm)
names(dfpop)=c("zip","totpop")
head(dfpop)

# get white population
fieldnm="P0100003"
dfpopwhite=getCensusData(APIkey,state,fieldnm)
names(dfpopwhite)=c("zip","popwhite")
head(dfpopwhite)

# get black population
fieldnm="P0100004"
dfpopblack=getCensusData(APIkey,state,fieldnm)
names(dfpopblack)=c("zip","popblack")
head(dfpopblack)

# get Asian population
fieldnm="P0100006"
dfpopasian=getCensusData(APIkey,state,fieldnm)
names(dfpopasian)=c("zip","popasian")
head(dfpopasian)


# Get data by ZCTA from 2011 ACS data (this is for all states)
getACSdata=function(fieldnm){
  resURL=paste("http://api.census.gov/data/2011/acs5?get=",fieldnm,"&for=zip+code+tabulation+area:*&key=",
               APIkey,sep="")
  df=fromJSON(resURL)
  df=df[2:length(df)]
  df_zip=as.character(sapply(df,function(x) x[2]))
  df_val=as.character(sapply(df,function(x) x[1]))
  df2=data.frame(zip=df_zip,val=as.numeric(df_val))
  df2=df2[!is.na(df2$val),]
  return(df2)
}

# Get median income
fieldnm="B19013_001E"
dfInc=getACSdata(fieldnm)
names(dfInc)=c("zip","medInc")
head(dfInc)


# get all census data together

zdata=merge(zipLoc2,dfage,by.x=c("ZCTA5"),by.y=c("zip"))
zdata=merge(zdata,dfpop,by.x=c("ZCTA5"),by.y=c("zip"))
zdata=merge(zdata,dfpopwhite,by.x=c("ZCTA5"),by.y=c("zip"))
zdata=merge(zdata,dfpopblack,by.x=c("ZCTA5"),by.y=c("zip"))
zdata=merge(zdata,dfpopasian,by.x=c("ZCTA5"),by.y=c("zip"))
zdata=merge(zdata,dfInc,by.x=c("ZCTA5"),by.y=c("zip"))
zdata$zip=zdata$ZCTA5
zdata$pctwhite=zdata$popwhite/zdata$totpop
zdata$pctblack=zdata$popblack/zdata$totpop
zdata$pctasian=zdata$popasian/zdata$totpop


summary(zdata$medAge)
hist(zdata$medAge)
zdata$medAgeLvl=cut(zdata$medAge,
                    breaks=c(0,35,40,45,50),
                    labels=c("<35","35-40","40-45",">45"))

summary(zdata$pctwhite)
hist(zdata$pctwhite)
zdata$pctwhiteLvl=cut(zdata$pctwhite,
                      breaks=c(0,0.2,0.5,0.8,0.9,1),
                      labels=c("<20%","20-50%","50-80%","80-90%",">90%"))

summary(zdata$pctblack)
hist(zdata$pctblack)
zdata$pctblackLvl=cut(zdata$pctblack,
                      breaks=c(0,0.2,0.5,0.8,1),
                      labels=c("<20%","20-50%","50-80%",">80%"))


summary(zdata$pctasian)
hist(zdata$pctasian)
zdata$pctasianLvl=cut(zdata$pctasian,
                      breaks=c(0,0.04,0.08,0.12),
                      labels=c("<4%","4-8%",">8%"))

summary(zdata$medInc)
hist(zdata$medInc)
zdata$medIncLvl=cut(zdata$medInc,
                    breaks=c(0,50000,75000,100000,120000),
                    labels=c("<50K","50-75K","75-100K",">100K"))

alphalvl=0.3

zipShp3$rnum=seq(1,nrow(zipShp3))
zipPlt=merge(zipShp3,zdata,by=c("zip"))
zipPlt=zipPlt[order(zipPlt$rnum),]

# choropleth map of median Age
x=get_googlemap(center="Dayton",maptype=c("roadmap"),zoom=9)

p1=ggmap(x)
p1=p1+geom_polygon(data=zipPlt,aes(x=long,y=lat,group=id,fill=medAgeLvl),color="black",alpha=0.2)
p1=p1+scale_fill_manual(values=rainbow(20)[c(4,8,12,16,20)])
p1=p1+labs(title="Median Age by Zip Code (US Census data)")
p1=p1+theme(legend.title=element_blank(),plot.title=element_text(face="bold"))
print(p1)

p2=ggmap(x)
p2=p2+geom_polygon(data=zipPlt,aes(x=long,y=lat,group=id,fill=medIncLvl),color="black",alpha=0.2)
p2=p2+scale_fill_manual(values=rainbow(20)[c(4,8,12,16,20)])
p2=p2+labs(title="Median Income by Zip Code (US census - ACS data)")
p2=p2+theme(legend.title=element_blank(),plot.title=element_text(face="bold"))
print(p2)

p3=ggmap(x)
p3=p3+geom_polygon(data=zipPlt,aes(x=long,y=lat,group=id,fill=pctwhiteLvl),color="black",alpha=0.2)
p3=p3+scale_fill_manual(values=rainbow(20)[c(4,8,12,16,20)])
p3=p3+labs(title="Median Income by Zip Code (US census - ACS data)")
p3=p3+theme(legend.title=element_blank(),plot.title=element_text(face="bold"))
print(p3)

p4=ggmap(x)
p4=p4+geom_polygon(data=zipPlt,aes(x=long,y=lat,group=id,fill=pctblackLvl),color="black",alpha=0.2)
p4=p4+scale_fill_manual(values=rainbow(20)[c(4,8,12,16,20)])
p4=p4+labs(title="Median Income by Zip Code (US census - ACS data)")
p4=p4+theme(legend.title=element_blank(),plot.title=element_text(face="bold"))
print(p4)

p5=ggmap(x)
p5=p5+geom_polygon(data=zipPlt,aes(x=long,y=lat,group=id,fill=pctasianLvl),color="black",alpha=0.2)
p5=p5+scale_fill_manual(values=rainbow(20)[c(4,8,12,16,20)])
p5=p5+labs(title="Median Income by Zip Code (US census - ACS data)")
p5=p5+theme(legend.title=element_blank(),plot.title=element_text(face="bold"))
print(p5)



# Get data from Zillow

## download demographics data to a folder

zwsid=zillowKey
zipList=zipLoc2$ZCTA5

for (i in 1:length(zipList)) {
  
  url=paste("http://www.zillow.com/webservice/GetDemographics.htm?zws-id=",zwsid,"&zip=",zipList[i],sep="")
  x=getURL(url)
  filenm=paste("zdata/zdemo",zipList[i],".xml",sep="")
  cat(x,file=filenm)
  
}

zdemodata=list(zip=character(),medListPrice=numeric(),medValSqFt=numeric(),
               pctgt2000=numeric(),pct8099=numeric(),pctsinglehome=numeric(),
               sizegt36=numeric(),size2436=numeric(),size1824=numeric())

for (i in 1:length(zipList)) {
  filenm=paste("zdata/zdemo",zipList[i],".xml",sep="")
  x=xmlInternalTreeParse(filenm)
  zdemodata$zip[i]=zipList[i]
  x2=xpathApply(x,"//table[name = 'Affordability Data']/data/attribute[name = 'Median List Price']/values/zip/value",xmlValue)
  zdemodata$medListPrice[i]=x2[[1]][1]
  x3=xpathApply(x,"//table[name = 'Affordability Data']/data/attribute[name = 'Median List Price Per Sq Ft']/values/zip/value",xmlValue)
  zdemodata$medValSqFt[i]=x3[[1]][1]
  x4=xpathApply(x,"//table[name='BuiltYear']/data/attribute[name='>2000']/value",xmlValue)
  zdemodata$pctgt2000[i]=ifelse(!is.null(x4),x4[[1]][1],0)
  x5=xpathApply(x,"//table[name='BuiltYear']/data/attribute[name='1980-1999']/value",xmlValue)
  zdemodata$pct8099[i]=ifelse(!is.null(x5),x5[[1]][1],0)
  x6=xpathApply(x,"//table[name='Census Summary-HomeType']/data/attribute[name='SingleFamily']/value",xmlValue)
  zdemodata$pctsinglehome[i]=ifelse(!is.null(x6),x6[[1]][1],0)
  x7=xpathApply(x,"//table[name='Census Summary-HomeSize']/data/attribute[name='>3600sqft']/value",xmlValue)
  zdemodata$sizegt36[i]=ifelse(!is.null(x7),x7[[1]][1],0)
  x8=xpathApply(x,"//table[name='Census Summary-HomeSize']/data/attribute[name='2400-3600sqft']/value",xmlValue)
  zdemodata$size2436[i]=ifelse(!is.null(x8),x8[[1]][1],0)
  x9=xpathApply(x,"//table[name='Census Summary-HomeSize']/data/attribute[name='1800-2400sqft']/value",xmlValue)
  zdemodata$size1824[i]=ifelse(!is.null(x9),x9[[1]][1],0)
  
  
}

zdemodata2=data.frame(zdemodata,stringsAsFactors=FALSE)
fldlist=names(zdemodata2)[2:length(names(zdemodata2))]
zdemodata2[,fldlist]=apply(zdemodata2[,fldlist],2,function(x) as.numeric(x))

zdemodata2$pctlt80=1-zdemodata2$pctgt2000-zdemodata2$pct8099
zdemodata2$sizegt24=zdemodata2$sizegt36+zdemodata2$size2436
zdemodata2$sizelt18=1-zdemodata2$size1824-zdemodata2$sizegt24

summary(zdemodata2)

getpctcut=function(x){
  getpctcut=cut(x,breaks=c(-0.001,0.25,0.5,0.75,1),
                labels=c("0-25%","25-50%","50-75%","75-100%"))
  getpctcut
}

# qc check
pickvar="pctlt80"
tmp=getpctcut(zdemodata2[,pickvar])
qc=data.frame(var=zdemodata2[,pickvar],grp=tmp)
qc%>%group_by(grp)%>%summarize(count=n(),varmin=min(var),varmax=max(var))

cutflds=names(zdemodata2)[4:length(names(zdemodata2))]
pctcutLvl=apply(zdemodata2[,cutflds],2,function(x) getpctcut(x))
colnames(pctcutLvl)=paste(colnames(pctcutLvl),"Lvl",sep="")

zdemodata2=cbind(zdemodata2,pctcutLvl)

# qc check
pickvar="pctgt2000"
grpvar=paste(pickvar,"Lvl",sep="")
qc=zdemodata2[,c(grpvar,pickvar)]
names(qc)=c("grpvar","var")
qc%>%group_by(grpvar)%>%summarize(count=n(),varmin=min(var),varmax=max(var))



zdemodata2$medListPriceLvl=cut(zdemodata2$medListPrice,
                               breaks=c(0,100000,200000,250000,300000,400000),
                               labels=c("<100K","100-200K","200-250K","250-300K",">300K"))
table(zdemodata2$medListPriceLvl)

zdemodata2$medValSqFtLvl=cut(zdemodata2$medValSqFt,
                             breaks=c(0,50,75,100,125,200),
                             labels=c("<50","50-75","75-100","100-125",">125"))
table(zdemodata2$medValSqFtLvl)

head(zdemodata2)

zdatafull=merge(zdata,zdemodata2,by=c("zip"))
head(zdatafull)

# save data
save(zdatafull,file="zdatafull.Rda")

zipShp3$rnum=seq(1,nrow(zipShp3))
zipPlt=merge(zipShp3,zdatafull,by=c("zip"))
zipPlt=zipPlt[order(zipPlt$rnum),]

# save data
save(zipPlt,file="zipPlt.Rda")

# choropleth map of median Age
x=get_googlemap(center="Dayton",maptype=c("roadmap"),zoom=9)

p3=ggmap(x)
p3=p3+geom_polygon(data=zipPlt,aes(x=long,y=lat,group=id,fill=medListPriceLvl),color="black",alpha=0.2)
p3=p3+scale_fill_manual(values=rainbow(20)[c(4,8,12,16,20)])
p3=p3+labs(title="Median Listing Price by Zip Code (from Zillow)")
p3=p3+theme(legend.title=element_blank(),plot.title=element_text(face="bold"))
print(p3)

p4=ggmap(x)
p4=p4+geom_polygon(data=zipPlt,aes(x=long,y=lat,group=id,fill=medValSqFtLvl),color="black",alpha=0.2)
p4=p4+scale_fill_manual(values=rainbow(20)[c(4,8,12,16,20)])
p4=p4+labs(title="Median Price Per SqFt by Zip Code (from Zillow)")
p4=p4+theme(legend.title=element_blank(),plot.title=element_text(face="bold"))
print(p4)
