#
# Scrape data on house listings from realtor.com
#

# set working directory
setwd("~/notesofdabbler/Rspace/dayoh_housing/")

# load libraries
library(rvest)
library(XML)
library(dplyr)

options(stringsAsFactors = FALSE)


#
# Search URL with following filter applied
# 3+ bedrooms, 2+ baths, 1800+ sqft, 0-20 years old
#
srchurl="http://www.realtor.com/realestateandhomes-search/Centerville_OH/beds-3/baths-2/sqft-8/pfbm-10/show-hide-pending"
srchurlList = rep(NA,10)
srchurlList[1] = srchurl
for (i in 2:length(srchurlList)){
  srchurlList[i] = paste(srchurl,"/pg-",i,sep="")
}

housedocList=as.list(rep(NA,length(srchurlList)))
for (i in 1:length(housedocList)){
  housedocList[[i]] = htmlTreeParse(srchurlList[i],useInternalNodes=TRUE)
}


getData = function(housedoc){
  

    ns_id = getNodeSet(housedoc,"//ul[@class='listing-summary']")   
  
    id = sapply(ns_id,function(x) xpathApply(x,".//li[@class='listing-location']//a[@href][1]",xmlGetAttr,"href"))
    id = unlist(id)

    staddr = sapply(ns_id,function(x) xpathApply(x,".//span[@class='listing-street-address']",xmlValue))
    staddr = sapply(staddr, function(x) ifelse(length(x) == 0,NA,x))
    staddr = unlist(staddr)

    city = sapply(ns_id,function(x) xpathApply(x,".//span[@class='listing-city']",xmlValue))
    city = sapply(city, function(x) ifelse(length(x) == 0,NA,x))
    city = unlist(city)
    
    zip = sapply(ns_id,function(x) xpathApply(x,".//span[@class='listing-postal']",xmlValue))
    zip = sapply(zip, function(x) ifelse(length(x) == 0,NA,x))
    zip = unlist(zip)

    st = sapply(ns_id,function(x) xpathApply(x,".//span[@class='listing-region']",xmlValue))
    st = sapply(st, function(x) ifelse(length(x) == 0,NA,x))
    st = unlist(st)
    
    price = sapply(ns_id,function(x) xpathApply(x,".//span[@class='listing-price blocker']|li[@class='listing-price indent-none listing-price-featured']",xmlValue))
    price = sapply(price, function(x) ifelse(length(x) == 0,NA,x))
    price = unlist(price) 
    
    beds = sapply(ns_id,function(x) xpathApply(x,".//span[@class='listing-beds']",xmlValue))
    beds = sapply(beds, function(x) ifelse(length(x) == 0,NA,x))
    beds = unlist(beds)

    baths = sapply(ns_id,function(x) xpathApply(x,".//span[@class='listing-baths']",xmlValue))
    baths = sapply(baths, function(x) ifelse(length(x) == 0,NA,x))
    baths = unlist(baths)

    housearea = sapply(ns_id,function(x) xpathApply(x,".//span[@class='listing-sqft'][1]",xmlValue))
    housearea = sapply(housearea, function(x) ifelse(length(x) == 0,NA,x))
    housearea = unlist(housearea)

    lotarea = sapply(ns_id,function(x) xpathApply(x,".//span[@class='listing-sqft'][2]",xmlValue))
    lotarea = sapply(lotarea, function(x) ifelse(length(x) == 0,NA,x))
    lotarea = unlist(lotarea)

    df = data.frame(url = id, staddr = staddr, city = city, zip = zip, st = st, price = price,
                    beds = beds, baths = baths, housearea = housearea, lotarea = lotarea)
    
    return(df)
    
}

housedataList = as.list(rep(NA,length(housedocList)))
for(i in 1:length(housedocList)){
    housedataList[[i]] = getData(housedocList[[i]])  
    print(i)
}

housedataDF = rbind_all(housedataList)
housedataDF$id = seq(1,nrow(housedataDF))

save(housedataDF,file="housedataDF.Rda")
#load(file="housedataDF.Rda")

# Get detailed info for a given house

gethouseDetail = function(id,housedoc){
    
    houseinfovar = housedoc %>% html_nodes(xpath = "//div[@id='GeneralInfo']//li[@class='list-sidebyside']/span[1]") %>% html_text()
    houseinfoval = housedoc %>% html_nodes(xpath = "//div[@id='GeneralInfo']//li[@class='list-sidebyside']/span[2]") %>% html_text()
    
    taxhistexist = length(housedoc %>% html_nodes(xpath = "//div[@id='PropertyHistory']"))
    
    if(taxhistexist > 0){
        taxinfoexist = length(housedoc %>% html_nodes(xpath = "//div[@id='PropertyHistory']/table[2]"))
      } else {
        taxinfoexist = 0
      }
    
    if(taxinfoexist > 0){
        housetaxyr = housedoc %>% html_nodes(xpath = "//div[@id='PropertyHistory']/table[2]/tbody/tr[1]/td[1]") %>% html_text()
        housetax = housedoc %>% html_nodes(xpath = "//div[@id='PropertyHistory']/table[2]/tbody/tr[1]/td[2]") %>% html_text()
        houseinfovar = c(houseinfovar,paste("tax",housetaxyr,sep=""))
        houseinfoval = c(houseinfoval,housetax)
    }
  
    if(taxhistexist > 0){
        househistexist = length(housedoc %>% html_nodes(xpath = "//div[@id='PropertyHistory']/table[2]"))
     } else {
        househistexist = 0 
     }
    
    if(househistexist > 0){
        househistdt = housedoc %>% html_nodes(xpath = "//div[@id='PropertyHistory']/table[1]/tbody/tr/td[1]") %>% html_text()
        househistevnt = housedoc %>% html_nodes(xpath = "//div[@id='PropertyHistory']/table[1]/tbody/tr/td[2]") %>% html_text()
        househistprice = housedoc %>% html_nodes(xpath = "//div[@id='PropertyHistory']/table[1]/tbody/tr/td[3]") %>% html_text()
    
        househistory = data.frame(dt=househistdt,evnt=househistevnt,price=househistprice)
        househistory$id =id
        
    } else {
      
        househistory = data.frame(dt="NA",evnt="NA",price="NA")
        househistory$id =id
      
    }
  
    houseinfo = data.frame(infovar=houseinfovar,
                         infoval=houseinfoval)
    houseinfo$id = id
  
  
  
    return(list(houseinfo = houseinfo, househistory = househistory))
    
}

houseurl = paste("http://www.realtor.com",housedataDF$url[1],sep="")
housedoc = html(houseurl)
test = gethouseDetail(1,housedoc)

# Get html document for each house
housedetaildocList = as.list(rep(NA,nrow(housedataDF)))
for(i in 1:length(housedetaildocList)){
    housedetailurl = paste("http://www.realtor.com",housedataDF$url[i],sep="")
    housedetaildocList[[i]]=html(housedetailurl)
    print(i)
}

houseinfoList=as.list(rep(NA,length(housedetaildocList)))
househistoryList=as.list(rep(NA,length(housedetaildocList)))

for(i in 1:length(houseinfoList)){
    housedetail = gethouseDetail(i,housedetaildocList[[i]])
    houseinfoList[[i]] = housedetail$houseinfo
    househistoryList[[i]] = housedetail$househistory
    print(i)
}

houseinfoDF = rbind_all(houseinfoList)
househistoryDF = rbind_all(househistoryList)

save(houseinfoDF,file="houseinfoDF.Rda")
save(househistoryDF,file="househistoryDF.Rda")
