#
#  Analyze AIChE 2014 tech program data
#

# set working directory
setwd("~/notesofdabbler/githubfolder/blog_notesofdabbler/exploreAICHE2014_techProgram/")

# load libraries
library(dplyr)
library(stringr)
library(tm)

# load data
load(file="paperInfo.Rda")
load(file="authdf.Rda")
load(file="affildf.Rda")

head(paperInfo)
head(authdf)
head(affildf)



sessions <- paperInfo%>%group_by(topic,session)%>%summarize(talks=n())
topics <- sessions%>%group_by(topic)%>%summarize(talks=sum(talks),sessions=n())

topics%>%arrange(desc(sessions))%>%select(topic,sessions)
topics%>%arrange(desc(talks))%>%select(topic,talks)

# select a topic of interest

pickTopic <- "Pharmaceutical Discovery, Development and Manufacturing Forum"
topicDetail <- paperInfo%>%filter(topic==pickTopic)
topicDetail_id <- as.data.frame(topicDetail$id)
names(topicDetail_id) <- c("id")

topicAuth <- inner_join(authdf,topicDetail_id,by=c("id"))
cntAuth <- topicAuth%>%group_by(authonly)%>%summarize(count=n())%>%arrange(desc(count))

topicAffil <- inner_join(affildf,topicDetail_id,by=c("id"))
cntAffil <- topicAffil%>%group_by(affilonly)%>%summarize(count=n())%>%arrange(desc(count))

company="[Ll]illy"
company="[Pp]fizer"              
company="[Ss]quib"
topicAffil_company <- topicAffil %>% filter(grepl(company,affilonly)) 
cnt_company <- length(unique(topicAffil_company$id))
cnt_company

# get list of universities
topicAffil_acad <- topicAffil %>% select(affilonly) %>% filter(grepl("[Uu]niv|[Tt]ech",topicAffil$affilonly)) %>% 
                      unique() 
topicAffil_acad$affilonly=str_trim(topicAffil_acad$affilonly)
topicAffil_acad

# manual clean
rmacad=c("Drug Product Science and Technology",
         "Drup Product Science  Technology",
         "Drug Product Science  Technology",
         "Institute of Research and Technology",
         "Department of Chemical Sciences and Technology",
         "Graduate School of Chemical Science and Technology"
         )
topicAffil_acad <- topicAffil_acad %>% filter(!(affilonly %in% rmacad))
topicAffil_acad$affilcln <- topicAffil_acad$affilonly
topicAffil_acad$affiltype <- "acad"
topicAffil_acad

# get list of companies
topicAffil_co <- topicAffil %>% select(affilonly) %>% filter(grepl("[Cc]ompany|[Ii]nc",topicAffil$affilonly)) %>% 
                             unique()
topicAffil_co
rmco=c("Inc","Princeton University","Princeton")
topicAffil_co <- topicAffil_co %>% filter(!(affilonly %in% rmco))
topicAffil_co
topicAffil_co$affilcln=c("Patheon",
                         "Eli Lilly",
                         "Eli Lilly",
                         "Dow Chemical",
                         "Scaleup Systems",
                         "Pfizer",
                         "Merck",
                         "BristolMyers",
                         "Abbvie",
                         "Amgen",
                         "Amgen",
                         "Colorcon")
topicAffil_co$affiltype="ind"
topicAffil_co

topicAffil_oth <- topicAffil$affilonly[grepl("pfizer|abbvie|lilly|bristolmyers|food|fda|glaxo",tolower(topicAffil$affilonly))]
topicAffil_oth <- unique(topicAffil_oth)
topicAffil_oth <- data.frame(affilonly=topicAffil_oth)

topicAffil_oth$affilcln=c("Pfizer",
                          "BristolMyers",
                          "BristolMyers",
                          "Eli Lilly",
                          "Eli Lilly",
                          "Eli Lilly",
                          "Dow Pharma",
                          "Dow Pharma",
                          "Dow Pharma",
                          "Pfizer",
                          "Pfizer",
                          "Dow Pharma",
                          "FDA",
                          "GSK",
                          "Pfizer",
                          "BristolMyers",
                          "Abbvie",
                          "FDA",
                          "Abbvie",
                          "Eli Lilly",
                          "Abbvie",
                          "FDA")
topicAffil_oth$affiltype="ind"                          


topicAffil_cln <- rbind(topicAffil_acad,topicAffil_co,topicAffil_oth)
topicAffil_cln <- unique(topicAffil_cln)
head(topicAffil_cln)


topicAffil2 <- left_join(topicAffil,topicAffil_cln,by=c("affilonly"))
topicAffil3 <- topicAffil2 %>% filter(!is.na(affilcln)) %>% group_by(id,affilcln,affiltype)%>%summarize()
head(topicAffil3)

missingTopics <- anti_join(topicDetail,topicAffil3,by=c("id"))

topicDetailwAffil <- inner_join(topicDetail,topicAffil3,by=c("id"))

table(topicDetailwAffil$affiltype)

sessionMix <- topicDetailwAffil %>% group_by(session,affiltype)%>%summarize(count=n())

# process titles
titles <- Corpus(VectorSource(topicDetail$title))
inspect(titles[1:5])

titles.cl <- tm_map(titles,tolower)
titles.cl <- tm_map(titles.cl,removePunctuation)
titles.cl <- tm_map(titles.cl,removeNumbers)

mystopwords=c(stopwords("english"))
titles.cl <- tm_map(titles.cl,removeWords,mystopwords)
titles.cl <- tm_map(titles.cl,PlainTextDocument)

myDTM <- DocumentTermMatrix(titles.cl)
myDTM.m <- as.matrix(myDTM)

tfreq=colSums(myDTM.m)
tfreq.df=data.frame(terms=names(tfreq),freq=tfreq)
tfreq.df=tfreq.df%>%arrange(desc(freq))

pca.fit=prcomp(myDTM.m)
plot(pca.fit)
summary(pca.fit)

load1=pca.fit$rotation[,3]
sort(abs(load1),decreasing=TRUE)[1:10]
