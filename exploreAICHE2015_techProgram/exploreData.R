#
# Explore AIChE 2015 technical program data for PD2M
#

library(dplyr)
library(tidyr)
library(ggplot2)

# load data
load(file = "talksdf.Rda") # talks
load(file = "authdf.Rda")  # authors
load(file = "talkdetaildf.Rda") # talk abstracts

affildf = read.csv("affildf_clean.csv",stringsAsFactors = FALSE) # list of organizations for each talk
affillist = read.csv("affillist.csv",stringsAsFactors = FALSE) # list of organizations

# General overiew

# number of sessions
sessions = data.frame(session = unique(talksdf$session))
nrow(sessions)

# number of talks
nrow(talksdf)

# number of authors
length(unique(authdf$author2))


# number of talks by author
authtalks = authdf %>% group_by(author2) %>% summarize(numtalks = n()) %>% arrange(desc(numtalks))

# Distribution of number of talks by a single author
distauthtalks = authtalks %>% group_by(numtalks) %>% summarize(numauthors = n())
ggplot(data = distauthtalks) + geom_bar(aes(x = numtalks,y = numauthors),stat = "identity",fill = "grey") +
  theme_bw()

# authors giving >= 4 talks
authtalks_ge4 = authtalks %>% filter(numtalks >= 4)
data.frame(authtalks_ge4)
ggplot(data = authtalks_ge4) + 
  geom_bar(aes(x = reorder(as.character(author2),numtalks),y = numtalks),stat = "identity",fill = "grey") + 
  xlab("") + 
  theme_bw() + coord_flip()

# number of organizations
nrow(affillist)
table(affillist$type)/nrow(affillist)



# Find number of talks by each organization
affildf2 = affildf %>% filter(affil2 != "Remove") %>% group_by(id,affil2) %>% summarize(cnt = n())

affildf3 = affildf2 %>% group_by(affil2) %>% summarize(numtalks = n()) %>% arrange(desc(numtalks))
affildf3 = inner_join(affildf3,affilist[,c("affil2","type")],by = "affil2")

distaffiltalks = affildf3 %>% group_by(numtalks) %>% summarize(numaffil = n())

ggplot(data = distaffiltalks) + geom_bar(aes(x = numtalks,y = numaffil),stat = "identity",fill = "grey") +
  theme_bw()

affildf3_ge5 = affildf3 %>% filter(numtalks >= 5)
ggplot(data = affildf3_ge5) + 
  geom_bar(aes(x = reorder(as.character(affil2),numtalks),y = numtalks,fill = type),stat = "identity") + 
  xlab("") + 
  theme_bw() + coord_flip()

# how many joint industry academic talks are there?
acadind = inner_join(affildf2,affilist[,c("affil2","type")],by = "affil2")
acadind2 = acadind %>% group_by(id,type) %>% summarize(cnt = n())
acadind2$flag = ifelse(acadind2$type == "acad",1,2)
acadind3 = acadind2 %>% group_by(id) %>% summarize(flagsum = sum(flag))

table(acadind3$flagsum)/nrow(acadind3)

chk_acadind3 = acadind3 %>% filter(flagsum == 3)

# Create a Rmd file with list of talks where both industry and academia are involved
chkdetail_acadind3 = inner_join(talksdf,chk_acadind3,by = "id")
chkdetail_acadind3 = chkdetail_acadind3 %>% select(id,title,talkurl)
affildf2_agg = affildf2 %>% group_by(id) %>% summarize(affil2 = paste(affil2,collapse = "; "))
chkdetail_acadind3 = inner_join(chkdetail_acadind3,affildf2_agg,by = "id")

outstr = as.character()
k = 1
for(i in 1:nrow(chkdetail_acadind3)){
  title = chkdetail_acadind3$title[i]
  talkurl = chkdetail_acadind3$talkurl[i]
  affilinfo = chkdetail_acadind3$affil2[i]
  outstr[k] = paste0("[",title,"](",talkurl,")   ")
  k = k + 1
  outstr[k] = paste0(affilinfo,"\n\n")
  k = k + 1
}
cat(paste(outstr,collapse = "\n"),file = "acadind.Rmd")

# check split between academia and industry in each session
acadind3_session = inner_join(acadind3,talksdf[,c("id","session")],by = "id")
acadind3_session2 = acadind3_session %>% group_by(session,flagsum) %>% summarize(cnt = n())
acadind3_session2s = spread(acadind3_session2,flagsum,cnt,fill = 0)

ggplot(data = acadind3_session2) + 
  geom_bar(aes(x = session, y = cnt, fill = factor(flagsum)),stat = "identity",position = "fill") + 
  xlab("") + ylab("") + 
  scale_fill_discrete(name = "",labels = c("acad","industry","acad and industry")) + 
  coord_flip() + theme_bw()
