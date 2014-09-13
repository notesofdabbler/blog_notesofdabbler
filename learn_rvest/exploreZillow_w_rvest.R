#
#  Explore Zillow data using rvest
# 
#  rvest package currently under development by Hadley Wickham
#  https://github.com/hadley/rvest
#

library(rvest)
library(magrittr)

# here the search is filtered to just homes for sale
# if there are less filters in search, the code will need to be modified since the css might be different
# for different types of results (eg. homes for sales vs new homes vs homes for rent)
url="http://www.zillow.com/homes/for_sale/Greenwood-IN/fsba,fsbo,fore,cmsn_lt/house_type/52333_rid/39.638414,-86.011362,39.550714,-86.179419_rect/12_zm/0_mmm/"

# get list of houses for sales that appears on the page
houselist <- url%>%html()%>%html_node("article")

# Extract zillow id for each listing
zpid <- houselist%>%html_attr("id")

# get the address for each listing
staddrlink <- houselist%>%html_node(".property-address a")%>%html_attr("href")
straddr <- sapply(strsplit(staddrlink,"/"),function(x) x[3])

# get the area of lot

lotsqfttxt <- lapply(houselist,function(x) x%>%html_node(".property-lot")%>%html_text())
# comment for above line: 
# Ideally I want to use the following line instead of the line above
#
# lotsqfttxt=houselist%>%html_node(".property-lot")%>%html_text()
#
# however if certain houses don't have .property-lot, 
# then it returns lesser number of entries than number of houses
# This makes combining this data with other house data difficult
#

## extract numerical value of lot area in sqft (do unit conversion from acre to sqft if needed)
lotsqft <- sapply(lotsqfttxt,
                  function(x) {
                     if(length(x)==0){
                       xsqftnum=NA
                       return(xsqftnum)
                     } else {
                       xsplit=strsplit(x," ")[[1]]
                       xsqftnum=as.numeric(gsub(",","",xsplit[1]))
                       if ((tolower(xsplit[2]) == "ac") | (tolower(xsplit[2]) == "acre")) {
                         xsqftnum=xsqftnum*43560
                       }  
                     }
                     
                     return(xsqftnum)
                  })


# year house was built
yrbuilt <- houselist%>%html_node(".property-year")%>%html_text()%>%gsub("Built in ","",.)%>%as.numeric()

# price of house
price <- houselist%>%html_node(".price-large")%>%html_text()%>%gsub("[\\$a-zA-Z,]","",.)%>%as.numeric()

# house parameters (number of beds, baths, house area)
houseparams <- houselist%>%html_node(".property-data")%>%html_text()
houseparamsSplit <- strsplit(houseparams,", ")
## get number of beds
numbeds <- sapply(houseparamsSplit,function(x) as.numeric(strsplit(x[1]," ")[[1]][1]))
## get number of baths
numbaths <- sapply(houseparamsSplit,function(x) as.numeric(strsplit(x[2]," ")[[1]][1]))
## get house area in sqft (convert from acre to sqft if needed)
housesqft <- sapply(houseparamsSplit,
               function(x){
                 xsplit=strsplit(x[3]," ")[[1]]
                 xsqft=as.numeric(gsub(",","",xsplit[1]))
                 if((tolower(xsplit[2]) == "ac") | (tolower(xsplit[2]) == "acre")) {
                   # convert acre to sqft
                   xsqft=xsqft*43560
                 }
                 return(xsqft)
               })

# bring all the house variables together into a dataframe
houseData <- data.frame(zpid,price,yrbuilt,numbeds,numbaths,housesqft,lotsqft,straddr)

head(houseData)

