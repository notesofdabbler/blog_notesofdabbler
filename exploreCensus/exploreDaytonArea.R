
# Explore Dayton area demographics and housing prices

# set working dir
setwd("~/notesofdabbler/githubfolder/blog_notesofdabbler/exploreCensus/")


# load libraries
library(XML)
library(RCurl)
library(stringr)
library(ggplot2)
library(maptools)
library(rgeos)
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

zipShp3=zipShp2[zipShp2$zip %in% zipLoc2$ZCTA5,]

save(zipShp3,file="zipshpDayton.Rda")
load(file="zipshpDayton.Rda")

x=get_googlemap(center="Dayton",maptype=c("roadmap"),zoom=9)
p=ggmap(x)
p=p+geom_polygon(data=zipShp3,aes(x=long,y=lat,group=id),fill="blue",color="black",alpha=0.2)
print(p)

# Get Census Data
APIkey="yourkey"

# state code (indiana)
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

# get median age from US census 2010 for a state
fieldnm="P0130001"
dfage=getCensusData(APIkey,state,fieldnm)
names(dfage)=c("zip","medAge")
head(dfage)

# Get median income by ZCTA from 2011 ACS data (this is for all states)
fieldnm="B19013_001E"
resURL=paste("http://api.census.gov/data/2011/acs5?get=",fieldnm,"&for=zip+code+tabulation+area:*&key=",
             APIkey,sep="")
dfInc=fromJSON(resURL)
dfInc=dfInc[2:length(dfInc)]
dfInc_zip=as.character(sapply(dfInc,function(x) x[2]))
dfInc_medinc=as.character(sapply(dfInc,function(x) x[1]))
dfInc2=data.frame(dfInc_zip,as.numeric(dfInc_medinc))
names(dfInc2)=c("zip","medInc")
dfInc2=dfInc2[!is.na(dfInc2$medInc),]
head(dfInc2)



zdata=merge(zipLoc2,dfage,by.x=c("ZCTA5"),by.y=c("zip"))
zdata=merge(zdata,dfInc2,by.x=c("ZCTA5"),by.y=c("zip"))
zdata$zip=zdata$ZCTA5

summary(zdata$medAge)
hist(zdata$medAge)

summary(zdata$medInc)
hist(zdata$medInc)

alphalvl=0.3

zdata$medAgeLvl=cut(zdata$medAge,
                    breaks=c(0,35,40,45,50),
                    labels=c("<35","35-40","40-45",">45"))

zdata$medIncLvl=cut(zdata$medInc,
                    breaks=c(0,50000,75000,100000,120000),
                    labels=c("<50K","50-75K","75-100K",">100K"))



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

# Get data from Zillow

## download demographics data to a folder

zwsid="yourid"
zipList=zipLoc2$ZCTA5

for (i in 1:length(zipList)) {
  
  url=paste("http://www.zillow.com/webservice/GetDemographics.htm?zws-id=",zwsid,"&zip=",zipList[i],sep="")
  x=getURL(url)
  filenm=paste("zdata/zdemo",zipList[i],".xml",sep="")
  cat(x,file=filenm)
  
}

zdemodata=list(zip=character(),medListPrice=numeric(),medValSqFt=numeric())

for (i in 1:length(zipList)) {
  filenm=paste("zdata/zdemo",zipList[i],".xml",sep="")
  x=xmlInternalTreeParse(filenm)
  zdemodata$zip[i]=zipList[i]
  x2=xpathApply(x,"//table[name = 'Affordability Data']/data/attribute[name = 'Median List Price']/values/zip/value",xmlValue)
  zdemodata$medListPrice[i]=x2[[1]][1]
  x3=xpathApply(x,"//table[name = 'Affordability Data']/data/attribute[name = 'Median Value Per Sq Ft']/values/zip/value",xmlValue)
  zdemodata$medValSqFt[i]=x3[[1]][1]
}

zdemodata2=data.frame(zdemodata,stringsAsFactors=FALSE)
zdemodata2$medListPrice=as.numeric(zdemodata2$medListPrice)
zdemodata2$medValSqFt=as.numeric(zdemodata2$medValSqFt)
summary(zdemodata2)
zdemodata2$medListPriceLvl=cut(zdemodata2$medListPrice,
                               breaks=c(0,100000,200000,250000,300000,400000),
                               labels=c("<100K","100-200K","200-250K","250-300K",">300K"))
table(zdemodata2$medListPriceLvl)

zdemodata2$medValSqFtLvl=cut(zdemodata2$medValSqFt,
                             breaks=c(0,50,75,100,125,200),
                             labels=c("<50","50-75","75-100","100-125",">125"))
table(zdemodata2$medValSqFtLvl)

head(zdemodata2)

zdata=merge(zdata,zdemodata2,by=c("zip"))
head(zdata)

zipShp3$rnum=seq(1,nrow(zipShp3))
zipPlt=merge(zipShp3,zdata,by=c("zip"))
zipPlt=zipPlt[order(zipPlt$rnum),]

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
