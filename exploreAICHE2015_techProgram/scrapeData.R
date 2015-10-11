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
  sessionauth = sessiondoc %>% html_nodes(xpath = "//div[@class = 'papertitle' and count(./a) > 1]//span[@class = 'paperauthors']") %>% html_text()
  
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
talksdf$id = seq(1,nrow(talksdf))

# conversion of accents and tildes in names
# helpful link at http://stackoverflow.com/questions/9511281/handling-special-characters-e-g-accents-in-r
#
talksdf$author2 = iconv(talksdf$author,"UTF-8","latin1")
talksdf$author3 = ifelse(is.na(talksdf$author2),talksdf$author,talksdf$author2)


# get individual author info for each talk

splitauth = function(auth){
   auth2 = gsub(" and ",",",auth)
   auth3 = str_split(auth2,",")
   auth3 = str_trim(auth3[[1]])
   return(auth3)
}

authL = lapply(talksdf$author3, function(x) splitauth(x))

authdfL = list()
for(i in 1:length(authL)){
  authdfL[[i]] = data.frame(author = authL[[i]])
  authdfL[[i]]$id = i
}
authdf = bind_rows(authdfL)

# check authors where encoding doesn't work
tmpqc = authdf
tmpqc$author2 = iconv(tmpqc$author,"UTF-8","latin1")
tmpqc2 = tmpqc[is.na(tmpqc$author2),]
unique(tmpqc2$author)

# One name appears multiple times due to encoding issues. Manually fix it
authdf$author2 = ifelse(authdf$author == "Salvador García-MuÃ±oz","Salvador García-Muñoz",authdf$author)

# get abstracts for talks
xtauthinfo = character()
abstract = character()
for(i in 1:nrow(talksdf)){
  doc = read_html(talksdf$talkurl[i])
  xtauthinfo[i] =  doc %>% html_nodes(".paperauthors") %>% html_text()
  getabstract = doc %>% html_nodes(".abstract") %>% html_text()
  if(length(getabstract) == 1){
      abstract[i] = getabstract
  } else {
      abstract[i] = ""
    }
  print(i)
}

talkdetaildf = data.frame(xtauthinfo = xtauthinfo, abstract = abstract)
talkdetaildf$id = seq(1:nrow(talkdetaildf))

# save datasets
save(talksdf,file = "talksdf.Rda")
save(authdf,file = "authdf.Rda")
save(talkdetaildf,file = "talkdetaildf.Rda")
