#
#   Scrape AIChE 2015 technical program data
#


# load libraries
library(rvest)
library(magrittr)
library(dplyr)
library(stringr)

# PD2M sessions link
urllink = "https://aiche.confex.com/aiche/2015/webprogram/26.html"

# get list of sessions in PD2M
doc = read_html(urllink)

sessions = doc %>% html_nodes(".itemtitle a") %>% html_text()
sessions

# links to sessions
sessionlinks = doc %>% html_nodes(".itemtitle a") %>% html_attr("href")
urlpre = "https://aiche.confex.com/aiche/2015/webprogram/"
sessionlinks = paste0(urlpre,sessionlinks)
sessionlinks


getSessionTalks = function(session,sessionlink){
  
  
  sessiondoc = read_html(sessionlink)
  
  # list of talks in the session
  sessiontalks = sessiondoc %>% html_nodes(xpath = "//div[@class = 'papertitle']//a[2]") %>% html_text()
  sessiontalks
  
  # authors of talks in the session
  sessionauth = sessiondoc %>% html_nodes(xpath = "//div[@class = 'papertitle']//span[@class = 'paperauthors']") %>% html_text()
  
  # links to talks in the session
  sessiontalklinks = sessiondoc %>% html_nodes(xpath = "//div[@class = 'papertitle']//a[2]") %>% html_attr("href")
  if(length(sessiontalklinks) > 0){
    sessiontalklinks = paste0(urlpre,sessiontalklinks)
    sessiontalklinks
  }
  
  if(length(sessiontalks) == 0){session = character(0)}
  
  sessiondf = data.frame(title = sessiontalks, author = sessionauth, talkurl = sessiontalklinks,session = session)
  
  return(sessiondf)
  
}

talksdfL = list()
for(i in 1:length(sessions)){
    sessionlink = sessionlinks[i]
    session = sessions[i]
    talksdfL[[i]] = getSessionTalks(session,sessionlink)
    print(i)
}
talksdf = bind_rows(talksdfL)
# conversion of accents and tildes in names
# helpful link at http://stackoverflow.com/questions/9511281/handling-special-characters-e-g-accents-in-r
#
talksdf$author = iconv(talksdf$author,"UTF-8","latin1")
talksdf$id = seq(1,nrow(talksdf))

# get individual author info for each talk

splitauth = function(auth){
   auth2 = gsub(" and ",",",auth)
   auth3 = str_split(auth2,",")
   auth3 = str_trim(auth3[[1]])
   return(auth3)
}

authL = lapply(talksdf$author, function(x) splitauth(x))

authdfL = list()
for(i in 1:length(authL)){
  authdfL[[i]] = data.frame(author = authL[[i]])
  authdfL[[i]]$id = i
}
authdf = bind_rows(authdfL)

# number of talks by author
authtalks = authdf %>% group_by(author) %>% summarize(numtalks = n()) %>% arrange(desc(numtalks))

xtauthinfo = character()
for(i in 1:nrow(talksdf)){
  xtauthinfo[i] = read_html(talksdf$talkurl[i]) %>% html_nodes(".paperauthors") %>% html_text()
  print(i)
}

xtauthdfL = list()
for(i in 1:length(xtauthinfo)){
  xtauthinfo2 = str_split(xtauthinfo[i],",")
  xtauthinfo3 = str_trim(xtauthinfo2[[1]])
  xtauthinfo4 = gsub("[0-9)(]","",xtauthinfo3)
  xtauthdfL[[i]] = data.frame(xtauthor = xtauthinfo4)
  xtauthdfL[[i]]$id = i
}
xtauthdf = bind_rows(xtauthdfL)
