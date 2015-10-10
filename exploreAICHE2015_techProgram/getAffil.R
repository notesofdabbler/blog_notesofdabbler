
#
# Get organization names associated with each talk
#

#
# Got some help from the following write up
# https://rpubs.com/lmullen/nlp-chapter
#

options(java.parameters = "-Xmx4g")

library(NLP)
library(openNLP)
library(StanfordCoreNLP)
library(stringr)
library(dplyr)


load("talkdetaildf.Rda")

xtauthinfo = as.character(talkdetaildf$xtauthinfo)


# remove and and numerical values from author/org name
cleanauth = function(auth){
  auth1 = gsub(" and ",",",auth)
  auth2 = gsub("[0-9)(]","",auth1)
  return(auth2)
}

# get affiliation using Stanford NLP NER
getAffil = function(auth){
  s = as.String(auth)
  doc = AnnotatedPlainTextDocument(s,p(s))
  
  a = doc$annotations[[1]]
  ner = lapply(a$features,function(x) x$NER)
  
  x = rep("",length(ner))
  for(i in 1:length(ner)){
    if(!is.null(ner[[i]])){
      if(ner[[i]][1] == "ORGANIZATION") {
        x[i] = s[a[i]]
      } else {
        x[i] = "+"
      }
    }
  }
  
  x2 = paste(x,collapse = " ")
  x3 = gsub("( \\+)+",";",x2)
  x4 = strsplit(x3,";")[[1]]
  x5 = str_trim(x4)
  x5 = x5[x5 != ""]
  return(x5)
  
}

p = StanfordCoreNLP_Pipeline(c("ner"))

affilL = list()
for(i in 1:length(xtauthinfo)){
  print(i)
  x = xtauthinfo[i]
  x = cleanauth(x)
  affil = getAffil(x)
  if(length(affil) == 0) affil = "NoExist"
  affilL[[i]] = data.frame(affil = as.vector(affil),stringsAsFactors = FALSE)
  affilL[[i]]$id = i
}
affildf = bind_rows(affilL)
affildf2 = affildf %>% group_by(id,affil) %>% summarize(cnt = n())

# write to disk for manual cleaning,
# cleaning inovlved going through the list and removing things wrongly flagged as organizations
# also removed variations of names (such as Pfizer R&D vs Pfizer Worldwide Research)
write.csv(affildf2,"affildf_clean.csv",row.names = FALSE)

# read in cleaned version
affildf3 = read.csv("affildf_clean.csv",stringsAsFactors = FALSE)

# get unique list of organizations
affillist = affildf3 %>% filter(affil2 != "Remove") %>% group_by(affil2) %>% summarize(cnt = n()) 

# write unique list of affiliations (to add labels of academia vs industry)
write.csv(affillist,"affillist.csv",row.names = FALSE)
