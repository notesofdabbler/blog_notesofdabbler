#
#  Get zip to county mapping
#

# set working dir
setwd("~/notesofdabbler/githubfolder/blog_notesofdabbler/exploreCensus/")

# load libraries
library(stringr)
library(zipcode)

# get state and county id-name maps
# http://www.census.gov/econ/cbp/download/georef02.txt
urlStateCty="http://www.census.gov/econ/cbp/download/georef02.txt"
stateCntyCodes = read.table(urlStateCty, sep = ",", colClasses = c("character"), 
                            header = TRUE)
head(stateCntyCodes)

# get zcta to county map
# http://www.census.gov/geo/maps-data/data/docs/rel/zcta_county_rel_10.txt

urlZipCnty="http://www.census.gov/geo/maps-data/data/docs/rel/zcta_county_rel_10.txt"
zipCnty = read.table(urlZipCnty, sep = ",", colClasses = c("character"), 
                     header = TRUE)
head(zipCnty)

# get zip to city map - from package zipcode
data(zipcode)
head(zipcode)

# merge zip-city with zip-county-state

zipMap = merge(zipCnty[, c("ZCTA5", "STATE", "COUNTY")], zipcode[, c("zip", 
                                                                     "city")], by.x = "ZCTA5", by.y = "zip")

# get names of county and state
zipMap2 = merge(zipMap, stateCntyCodes, by.x = c("STATE", "COUNTY"), by.y = c("fipstate", 
                                                                              "fipscty"))
zipMap2$stname = sapply(zipMap2$ctyname, function(x) str_split(x, ",")[[1]][2])

# save the dataset
save(zipMap2, file = "zipCityCountyStateMap.Rda")
