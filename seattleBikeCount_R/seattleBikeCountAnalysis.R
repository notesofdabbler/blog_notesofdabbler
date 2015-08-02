#' # Seattle's Fremont Bridge Bicyclists Again in the News
#'
#' Back in 2013, David had done analysis of [bicycle trips](http://www.r-bloggers.com/fun-with-fremont-bridge-bicyclists/) 
#' across Seattle's Fremont bridge. More recently, [Jake Vanderplas](http://www.astro.washington.edu/users/vanderplas/) (creator of Python's 
#' very popular [Scikit-learn](http://scikit-learn.org/stable/) package)
#' wrote a very nice [blog post](https://jakevdp.github.io/blog/2015/07/23/learning-seattles-work-habits-from-bicycle-counts/) on 
#' Learning Seattle Work habits from bicycle counts across Fremont bridge.
#'
#' I wanted to work through Jake's analysis using R since I am learning R. Please read the original article by Jake to 
#' get the full context and thinking behind the analysis. For folks interested in Python, Jake has provided link in the 
#' blog post to iPython notebook where you can work through the analysis in Python 
#' (and learn some key Python modules: pandas, matplotlib, sklearn along the way)
#'
#+ setup, include=FALSE
library(knitr)
opts_chunk$set(fig.height = 8,fig.width = 8)


#+
# load libraries
library(dplyr) # data munging
library(tidyr) # data munging
library(ggplot2) # plotting
library(lubridate) # work with dates
library(mclust) # for clustering using gaussian mixture model
library(Cairo) # for getting png outputs
library(rje) # to use cubehelix colors for plots
library(timeDate) # get list of US holidays


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

#' ## Weekly Trend of Bicycle Counts
#+
# sum by week and plot trend
# In Python with Pandas in Jake's post, from data set to this plot was a single line of code 
# some of the things I am doing on date processing might have better ways to code
rawcounts_wk = rawcounts %>% group_by(Weekchr) %>% summarize(West = sum(West), East = sum(East), total = sum(total))
rawcounts_wk$Week = as.Date(rawcounts_wk$Weekchr)
rawcounts_wk2 = gather(rawcounts_wk,var,val,-Week,-Weekchr)

#+ fig.height = 8,fig.width = 10
#CairoPNG("bikecounts_wk.png",width = 600, height = 480)
ggplot(rawcounts_wk2) + geom_line(aes(x = Week,y = val,color = var)) + xlab("Date") + ylab("Weekly Trips") + theme_bw()
#dev.off()

#' ## Principal Component Analysis of Data
#+
# spread data so that each row is a day and columns are bike trips for each hr (24 points) both for east and west side
rawcounts_gloc = gather(rawcounts,loc,val,East,West)
rawcounts_gloc$key = paste(rawcounts_gloc$loc,rawcounts_gloc$hr,sep = "_")
rawcounts_gloc = rawcounts_gloc %>% select(Date,key,val)
rawcounts_day = spread(rawcounts_gloc,key,val)

nameorder = c("Date",paste0("East_",seq(1,24)),paste0("West_",seq(1,24)))
rawcounts_day = rawcounts_day[,nameorder]

head(rawcounts_day)
dim(rawcounts_day)

# Principal component analysis of the data where each row is a day and data for each day is the 24 hr bike counts on the
# east and west side
# X is the numeric marix of bike counts for each day
X = rawcounts_day[,2:ncol(rawcounts_day)]
X = as.matrix(X)
X[is.na(X)] = 0
pca = prcomp(X)

# Summary of the PCA
summary(pca)
screeplot(pca,type = "lines")

# First two PC's are kept as in Jake's post (they account for 90% of the variance)
scores = data.frame(pca$x[,1:2])

# The scores are plotted on a 2D plot and each point (day) is colored based on total number of trips that day

scores$tottrips = apply(X,1,sum)

ggplot(data = scores,aes(x = PC1,y = PC2,color = tottrips)) + geom_point()

# the color scheme in the plot in Jake's post (cubehelix) seems much nicer. 
# But since I wasn't sure how to use it with continuous scale, I discretized total trips into buckets
# and applied that color scheme
#CairoPNG("pcScores.png",height = 600, width = 600)
ggplot(data = scores,aes(x = PC1,y = PC2,color = cut(tottrips,7))) + geom_point() + 
  scale_color_manual(name = "total trips", values = cubeHelix(7)) 
#dev.off()

# In Jake's post, there was not much discussion on loadings. Some of the subsequent inferences done using clustering can be found
# from the loadings of the first 2 principal components also (at least in this case)
loadings = as.data.frame(pca$rotation[,1:2])
loadings$label = row.names(loadings)
loadings$loc = sapply(loadings$label,function(x) strsplit(x,"_")[[1]][1])
loadings$hr = rep(seq(1,24),2)

head(loadings)

loadings_gpc = gather(loadings,pc,val,PC1,PC2)

#+ fig.height = 8,fig.width = 10
# the loadings plot below also shows what the clustering later shows that PC1 is about weekday bike riding patterns
# and PC2 is about holiday bike riding patters
# PC1 also shows what clustering shows later that West side peaks in the morning and East side peaks in the evening,
#CairoPNG("pcLoadings.png",height = 600, width = 800)
ggplot(data = loadings_gpc) + geom_line(aes(x = hr, y = val, color = loc)) + facet_grid(~pc,scales = "free_y") + theme_bw()
#dev.off()

#' ## Clustering

#+
# clustering: Gaussian mixture model clustering done with two PC scores as in Jake's post
#
fitclus = Mclust(scores[,c("PC1","PC2")],G = 2)

# append cluster information to scores data
scores$clus = fitclus$classification

# plot first 2 PC scores and color by cluster (the clustering in this case picks the two clusters seen visually)
ggplot(data = scores,aes(x = PC1,y = PC2,color = clus)) + geom_point() + theme_bw()

# append cluster information to the original data used for PCA (day x hr data set)
rawcounts_day$clus = scores$clus

# merge cluster info back to original data
rawcounts_wclus = inner_join(rawcounts,rawcounts_day[,c("Date","clus")],by = "Date")

# get the average number of bike rides for each cluster
rawcounts_wclus_gloc = gather(rawcounts_wclus[,c("Date","East","West","total","hr","clus")],loc,val,East,West,total)
rawcounts_clusavg = rawcounts_wclus_gloc %>% group_by(clus,loc,hr) %>% summarize(val = mean(val))

#+ fig.height = 8,fig.width = 10
#
# This type of graph was used in the original post to show the difference in trend between two clusters
# that led next to investigating the day of week in each cluster
#
#CairoPNG("clusterAvg.png",height = 600, width = 800)
ggplot(data = rawcounts_clusavg) + geom_line(aes(x = hr,y = val,color = loc)) + facet_grid(~clus) + 
  ylab("Avg bike trips") + theme_bw()
#dev.off()

# get weekday for each date
scores$weekday = wday(rawcounts_day$Date,label = TRUE)

# plot 2 PC scores and color them by day of week
# This plot gives interpretation to the clusters as weekday cluster (1) and weekend cluster (2)
#CairoPNG("scoresbyWkday.png",height = 600, width = 600)
ggplot(data = scores,aes(x = PC1,y = PC2,color = weekday)) + geom_point() + theme_bw()
#dev.off()


# Next the few weekdays that fall with weekend cluster were investigated
oddWkdays = data.frame(Date = rawcounts_day$Date,clus = scores$clus,weekday = scores$weekday)
oddWkdays = oddWkdays %>% filter(clus == 2,!(weekday %in% c("Sat","Sun")))
oddWkdays$Date = as.Date(oddWkdays$Date)

head(oddWkdays)

# Get master holiday list (names) and associated dates in year 2012-2015
mstrHolidayList = expand.grid(year = 2012:2015,holidayName = as.character(listHolidays("US")),stringsAsFactors = FALSE)
HolidayDates = lapply(1:nrow(mstrHolidayList),function(x) holiday(year = mstrHolidayList$year[x], Holiday = mstrHolidayList$holidayName[x])@Data)
mstrHolidayList$Date = do.call("c",lapply(HolidayDates,function(x) as.Date(x)))

# add day before and day after
mstrHolidayList_plusOne = data.frame(year = mstrHolidayList$year, Date = mstrHolidayList$Date + 1, 
                                     holidayName = paste0("day after ",mstrHolidayList$holidayName))
mstrHolidayList_minusOne = data.frame(year = mstrHolidayList$year, Date = mstrHolidayList$Date - 1, 
                                     holidayName = paste0("day before ",mstrHolidayList$holidayName))

mstrHolidayList = bind_rows(mstrHolidayList,mstrHolidayList_plusOne,mstrHolidayList_minusOne)

# check how many of the odd week days that fall in cluster 2 are holidays
chkoddWkdays = (oddWkdays$Date %in% mstrHolidayList$Date)
# The total below is zero indicating that all of the odd Week days are US holidays
sum(chkoddWkdays == FALSE)

# list of holiday names
oddWkdaysname = mstrHolidayList$holidayName[match(oddWkdays$Date,mstrHolidayList$Date)]
oddWkdaysname = unique(gsub("(day before |day after )","",oddWkdaysname))
# these are holidays when people didn't go to work
oddWkdaysname

# US holidays which did not fall in the odd cluster (i.e. people went to work)
holidayNames =  mstrHolidayList$holidayName
holidayNames =  unique(gsub("(day before |day after )","",holidayNames))

# these are holidays when people went to work
workingHolidays = holidayNames[!(holidayNames %in% oddWkdaysname)]
workingHolidays

#' Thanks again to [Jake Vanderplas](http://www.astro.washington.edu/users/vanderplas/) for a nice analysis and illustrating how lot 
#' lot of insights can be gathered from data.
#'
#' ## Session Info
#' This analysis was done in RStudio v0.99.465
#+
sessionInfo()