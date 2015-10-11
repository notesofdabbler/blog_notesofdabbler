#
# Explore AIChE 2015 technical program data for PD2M
#

library(dplyr)
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
