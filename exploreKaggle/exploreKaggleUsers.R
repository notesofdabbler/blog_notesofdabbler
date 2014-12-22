
# set working directory
setwd("~/notesofdabbler/githubfolder/blog_notesofdabbler/exploreKaggle/")

# load libraries
library(rvest)
library(stringr)
library(dplyr)
library(googleVis)
library(ggplot2)

url = "http://www.kaggle.com/users"
urllist = paste(url,seq(2,13),sep="?page=")
urllist = c(url,urllist)
head(urllist)

getURL = function(url){
    users = html(url) %>% html_nodes(".users-list")
  
    usrname = users %>% html_nodes(".profilelink") %>% html_text()
    usrrnk = users %>% html_nodes(".rank") %>% html_text()
    usrnumcomp = users %>% html_nodes(".comps") %>% html_text()
    usrloc = users %>% html_nodes("li") %>% html_text()
    usrloc2 = sapply(as.list(usrloc),function(x) {
        xsplit = strsplit(x,"\r\n")[[1]]
        xtrim = str_trim(xsplit)
        xnoblank = xtrim[xtrim != ""]
        xloc = xnoblank[length(xnoblank)]
        return(xloc)
      })
    usrdf = data.frame(usrname,usrrnk,usrnumcomp,usrloc2)
    return(usrdf)
}

usrdf = list()
length(usrdf) = length(urllist)

for(i in 1:length(urllist)){
  usrdf[[i]] = getURL(urllist[i])
}

usrdf2 = rbind_all(usrdf)

tmpqc = usrdf2[grepl("competition",usrdf2[["usrloc2"]]),]
usrdf2[["usrloc2"]][grepl("competition",usrdf2[["usrloc2"]])] = "unknown"
usrdf2["usrrnk2"] = as.numeric(gsub("[^(0-9)]","",usrdf2[["usrrnk"]]))
usrdf2["usrnumcomp2"] = as.numeric(gsub("[^(0-9)]","",usrdf2[["usrnumcomp"]]))


cntrycnt = usrdf2 %>% group_by(usrloc2) %>% summarize(cntrycnt = n()) %>% arrange(desc(cntrycnt))

cntrylist = cntrycnt$usrloc2

write.csv(cntrylist,"cntrylist.csv")

cntrylist_cleaned = read.csv("cntrylist_cleaned.csv",sep=",")

usrdf3 = merge(usrdf2,cntrylist_cleaned,by.x=c("usrloc2"),by.y=c("cntryName"))
cntrycnt3 = usrdf3 %>% group_by(cntryName2)%>%
                  summarize(cntrycnt = n(),avgrnk = mean(usrrnk2),bestrnk = min(usrrnk2),
                            totcomp = sum(usrnumcomp2)) %>% 
                  arrange(bestrnk)
cntrycnt3["compperusr"] = cntrycnt3[["totcomp"]]/cntrycnt3[["cntrycnt"]]

G = gvisGeoChart(cntrycnt3, locationvar = "cntryName2",
                        colorvar = "compperusr",options=list(height=600,width=600))
T = gvisTable(cntrycnt3,options = list(height=600,width=600))

GT = gvisMerge(G,T,horizontal = TRUE)
plot(GT)


# Get list of competitions
localfile = "test2.html"

compdata = html(localfile)

compnames = compdata %>% html_nodes(".competition-details h4") %>% html_text()
complinks = compdata %>% html_nodes(".competition-details") %>% html_node("a") %>% html_attr("href") 
comptype = compdata %>% html_nodes(xpath = "//tr//td[2]") %>% html_text()

comptype2 = comptype
comptype2[grepl("[$]",comptype)] = "prizeCompetition"

compprize = 0
compprize = ifelse(comptype2 == "prizeCompetition",comptype,0)
compprize = as.numeric(gsub("[$,]","",compprize))

compnumteams = compdata %>% html_nodes(xpath = "//tr//td[3]") %>% html_text()

compdf = data.frame(compnames,complinks,comptype,comptype2,compprize,compnumteams,duration,stringsAsFactors = FALSE)

compdf[["compnumteams"]] = as.numeric(compdf[["compnumteams"]])


getDays = function(htmlnode){
  txt = htmlnode %>% html_text()
  txtlefttrim = gsub("^.*\\(","",txt)
  txtrttrim = gsub("\\).*$","",txtlefttrim)
  numdays = gsub("[,a-zA-Z ]","",txtrttrim)
  numdays = as.numeric(numdays)
  return(numdays)
}

duration = rep(0,length(complinks))
for(i in 1:length(complinks)){
  comppg = html(complinks[i])
  durationNode = comppg %>% html_nodes("#end-time-note")
  if(length(durationNode) > 0){
    duration[i] = getDays(durationNode)
  }
  print(i)  
}


compdf[["pop.html.tooltip"]] = paste(compdf[["compnames"]],"</br>",
                                     "Prize ($):",compdf[["compprize"]],"</br>",
                                     "Duration (days):",compdf[["duration"]],"</br>",
                                     "Number of teams:",compdf[["compnumteams"]],sep="")


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



