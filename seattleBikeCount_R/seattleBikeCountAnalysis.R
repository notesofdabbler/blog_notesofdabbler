#
#
# Jake Vanderplas recently wrote a very nice article on Learning Seattle Work habits from bicycle counts
#  https://jakevdp.github.io/blog/2015/07/23/learning-seattles-work-habits-from-bicycle-counts/
#
# I was trying to see if I can replicate the analysis in R
#

# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(mclust)


# load data
# 
rawcounts = read.csv("https://data.seattle.gov/api/views/65db-xm6k/rows.csv?accessType=DOWNLOAD")


# inspect data
str(rawcounts)
head(rawcounts)

# rename columns, set missing values to zero and create a total column (total of east and west)
names(rawcounts) = c("Datetime","West","East")
rawcounts[is.na(rawcounts)] = 0
rawcounts$total = rawcounts$West + rawcounts$East

# Date processing (get date, week, time)
rawcounts$Datetime = as.POSIXct(strptime(rawcounts$Datetime,"%m/%d/%Y %H:%M:%S"))
rawcounts$Date = as.POSIXct(strptime(rawcounts$Datetime,"%Y-%m-%d"))
rawcounts$Time = strftime(rawcounts$Datetime,"%H:%M:%S")
rawcounts$Week = rawcounts$Date - as.difftime(wday(rawcounts$Date)-1,unit = "days")
rawcounts$Weekchr = as.Date(rawcounts$Week)
rawcounts = rawcounts[!is.na(rawcounts$Time),]
rawcounts = rawcounts %>% group_by(Date) %>% mutate(hr = seq(1,length(Date)))

head(rawcounts)

# sum by week and plot trend
rawcounts_wk = rawcounts %>% group_by(Weekchr) %>% summarize(West = sum(West), East = sum(East), total = sum(total))
rawcounts_wk$Week = as.Date(rawcounts_wk$Weekchr)
rawcounts_wk2 = gather(rawcounts_wk,var,val,-Week,-Weekchr)

ggplot(rawcounts_wk2) + geom_line(aes(x = Week,y = val,color = var)) + xlab("Date") + ylab("Weekly Trips") + theme_bw()

# spread data so that each row is a day and columns are bike trips for each hr (24 points) both for east and west side
rawcounts2 = gather(rawcounts,loc,val,East,West)
rawcounts2$key = paste(rawcounts2$loc,rawcounts2$hr,sep = "_")
rawcounts2 = rawcounts2 %>% select(Date,key,val)
rawcounts3 = spread(rawcounts2,key,val)

nameorder = c("Date",paste0("East_",seq(1,24)),paste0("West_",seq(1,24)))
rawcounts3 = rawcounts3[,nameorder]

head(rawcounts3)
dim(rawcounts3)

# Principal component analysis
X = rawcounts3[,2:ncol(rawcounts3)]
X = as.matrix(X)
X[is.na(X)] = 0
pca = prcomp(X)

summary(pca)
screeplot(pca,type = "lines")

scores = data.frame(pca$x[,1:2])
scores$tottrips = apply(X,1,sum)


ggplot(data = scores,aes(x = PC1,y = PC2,color = tottrips)) + geom_point() + theme_bw()

loadings = pca$rotation[,1:2]

# clustering
fitclus = Mclust(scores[,c("PC1","PC2")],G = 2)

summary(fitclus)

scores$clus = fitclus$classification

ggplot(data = scores,aes(x = PC1,y = PC2,color = clus)) + geom_point() + theme_bw()

rawcounts3$clus = scores$clus

# merge cluster info back to original data

rawcounts4 = inner_join(rawcounts,rawcounts3[,c("Date","clus")],by = "Date")

rawcounts5 = gather(rawcounts4[,c("Date","East","West","total","hr","clus")],loc,val,East,West,total)
rawcounts6 = rawcounts5 %>% group_by(clus,loc,hr) %>% summarize(val = mean(val))

ggplot(data = rawcounts6) + geom_line(aes(x = hr,y = val,color = loc)) + facet_grid(~clus) + theme_bw()

scores$weekday = wday(rawcounts3$Date,label = TRUE)

ggplot(data = scores,aes(x = PC1,y = PC2,color = weekday)) + geom_point() + theme_bw()

scores$friday = ifelse(scores$weekday == "Fri","Fri","Othr")

ggplot(data = scores,aes(x = PC1,y = PC2,color = friday)) + geom_point() + theme_bw()
