##
## Explore Kaggle competition data
##

# set working directory
setwd("~/notesofdabbler/githubfolder/blog_notesofdabbler/exploreKaggle/")

# load libraries
library(rvest)
library(dplyr)
library(googleVis)
library(ggplot2)

# Get list of competitions
localfile = "kaggleCompetitionList.html"
compdata = html(localfile)

# get names of competitions
compnames = compdata %>% html_nodes(".competition-details h4") %>% html_text()
# get links for each competition
complinks = compdata %>% html_nodes(".competition-details") %>% html_node("a") %>% html_attr("href") 
# get the competition type (knowledge, job or prize amount)
comptype = compdata %>% html_nodes(xpath = "//tr//td[2]") %>% html_text()

# Assign prizeCompetition label to competitions that have prizes
comptype2 = comptype
comptype2[grepl("[$]",comptype)] = "prizeCompetition"

# get numeric value of prize for prize competitions and set to 0 for other competition types
compprize = ifelse(comptype2 == "prizeCompetition",comptype,0)
compprize = as.numeric(gsub("[$,]","",compprize))

# get number of teams
compnumteams = compdata %>% html_nodes(xpath = "//tr//td[3]") %>% html_text()
compnumteams = as.numeric(compnumteams)

# combine into a dataframe
compdf = data.frame(compnames,complinks,comptype,comptype2,compprize,compnumteams,stringsAsFactors = FALSE)

head(compdf)

# function to extract number of days a competition was run
# input is the specific page for the competition
# the total days is in parenthesis and regex is used to extract that
getDays = function(htmlnode){
  txt = htmlnode %>% html_text()
  txtlefttrim = gsub("^.*\\(","",txt)
  txtrttrim = gsub("\\).*$","",txtlefttrim)
  numdays = gsub("[,a-zA-Z ]","",txtrttrim)
  numdays = as.numeric(numdays)
  return(numdays)
}

# Get duration of each competition

duration = rep(0,length(complinks))
for(i in 1:length(complinks)){
  comppg = html(complinks[i])
  durationNode = comppg %>% html_nodes("#end-time-note")
  if(length(durationNode) > 0){
    duration[i] = getDays(durationNode)
  }
#  print(i)  
}

compdf["duration"] = duration

# create a field to show as tooltip in googleVis scatter plot
# this has the following information:
# Competition name, prize, number of teams
compdf[["pop.html.tooltip"]] = paste(compdf[["compnames"]],"</br>",
                                     "Prize ($):",compdf[["compprize"]],"</br>",
                                     "Duration (days):",compdf[["duration"]],"</br>",
                                     "Number of teams:",compdf[["compnumteams"]],sep="")

## Make plots
# plot of number of teams vs prize
pltdf = compdf[,c("compprize","compnumteams","pop.html.tooltip")] %>% filter(compprize > 0)
plt = gvisScatterChart(pltdf,options=list(tooltip="{isHtml:'true'}",
                                          explorer="{actions: ['dragToZoom', 
                                          'rightClickToReset'],
                                          maxZoomIn:0.05}",
                                          vAxis="{title:'# teams'}",
                                          hAxis="{title:'Prize ($)'}",
                                          width=600,height=600))
plot(plt)


# plot of duration vs prize
pltdf = compdf[,c("compprize","duration","pop.html.tooltip")] %>% filter(compprize > 0,duration > 0)
plt = gvisScatterChart(pltdf,options=list(tooltip="{isHtml:'true'}",
                                          explorer="{actions: ['dragToZoom', 
                                          'rightClickToReset'],
                                           maxZoomIn:0.05}",
                                          vAxis="{title:'Duration (days)'}",
                                          hAxis="{title:'Prize ($)'}",
                                          width=600,height=600))
plot(plt)

# plot of duration vs number of teams
pltdf = compdf[,c("duration","compnumteams","pop.html.tooltip")] %>% filter(compprize > 0,duration > 0)
plt = gvisScatterChart(pltdf,options=list(tooltip="{isHtml:'true'}",
                                          explorer="{actions: ['dragToZoom', 
                                          'rightClickToReset'],
                                          maxZoomIn:0.05}",
                                          vAxis="{title:'# teams'}",
                                          hAxis="{title:'Duration (days)'}",
                                          width=600,height=600))
plot(plt)


ggplot(data=compdf %>% filter(comptype2 == "Knowledge"),
       aes(x=reorder(compnames,compnumteams),y=compnumteams))+geom_bar(stat="identity")+
       xlab("")+ylab("# Teams")+
       coord_flip()+theme_bw(20)
