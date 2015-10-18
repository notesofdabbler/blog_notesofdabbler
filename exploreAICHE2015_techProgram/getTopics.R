#
#  Get topics from abstracts of talks
#

# load libraries
library(topicmodels) # used to determine topics
library(tm) # used for text processing
library(dplyr) # data munging
library(tidyr) # data munging
library(ggplot2) # plotting
library(LDAvis) # visualizing LDA results
library(Cairo) # output plots
library(scales) # formatting of ggplot2 plot lables

# load data
load(file = "talksdf.Rda") # talks
load("talkdetaildf.Rda") # talk abstracts
affildf = read.csv("affildf_clean.csv",stringsAsFactors = FALSE) # list of organizations for each talk
affillist = read.csv("affillist.csv",stringsAsFactors = FALSE) # list of organizations

##------------------ Get data ready for text analysis -------------

# Lot of code adapated from
# https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf


abstracts = talkdetaildf$abstract

doc = VCorpus(VectorSource(abstracts))
doc
inspect(doc[1:3])

myStopwords = c(stopwords("english"),"using","results","pharmaceutical","can","used","will")

doc2 = tm_map(doc,stripWhitespace)
doc2 = tm_map(doc2,content_transformer(tolower))
doc2 = tm_map(doc2,removeWords,myStopwords)
doc2 = tm_map(doc2,removePunctuation)
doc2 = tm_map(doc2,removeNumbers)
#doc2 = tm_map(doc2,stemDocument)

dtm = DocumentTermMatrix(doc2)

# find frequent terms
termFreq = colSums(as.matrix(dtm))
termFreqDesc = sort(termFreq,decreasing = TRUE)
head(termFreqDesc,30)
table(termFreq)

# keep words with at least a certain count
mincnt = 10
dtm_m = as.matrix(dtm)
dtm_m2 = dtm_m[,termFreq >= mincnt]
dim(dtm_m2)

# check frequent terms again
termFreq_chk = colSums(dtm_m2)
termFreq_chk = sort(termFreq_chk,decreasing = TRUE)
head(termFreq_chk,30)

# keep rows that have at least one word from the truncated word list
rowsum_chk = rowSums(dtm_m2)
dtm_m2 = dtm_m2[rowsum_chk > 0,]
dim(dtm_m2)

##----------------------- Fit topic model -----------------------

# Lot of code adapated from
# https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf

numtopics = 10
SEED = 2345
fittopic = LDA(dtm_m2, k = numtopics, control = list(seed = SEED))
Terms = terms(fittopic,20)

# plot the top likely words in each topic
dfTerms = data.frame(Terms)
dfTerms$num = seq(nrow(dfTerms),1)
dfTerms_g = gather(dfTerms,topic,term,-num)
dfTerms_g$x = 1

CairoPNG("topicTerms.png",height = 600,width = 1200)
ggplot(dfTerms_g) + geom_text(aes(x = x,y = num,label = term)) + facet_grid(~topic) + 
  xlab("") + ylab("") + 
  theme_bw() + 
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# Give names to the topics
topicnames = c("bio","crystallization","process dev/modeling","HME","crystallization 2","dissolution/enabled formulation",
               "formulation","modeling","granulation/mixing","continuous")
names(topicnames) = paste0("X",seq(1,10))

# visualize topics
# http://cpsievert.github.io/LDAvis/reviews/reviews.html
postprob = posterior(fittopic)$topics
postprobw = posterior(fittopic)$terms
doc.length = apply(dtm_m2,1,sum)
vocab = colnames(dtm_m2)
term.frequency = apply(dtm_m2,2,sum)

json <- createJSON(phi = postprobw, 
                   theta = postprob, 
                   doc.length = doc.length, 
                   vocab = vocab, 
                   term.frequency = term.frequency)
#serVis(json)

# get likelytopic and 2nd likelytopic for each talk
likelytopic1 = rep(0,nrow(postprob))
probtopic1 = rep(0,nrow(postprob))
likelytopic2 = rep(0,nrow(postprob))
probtopic2 = rep(0,nrow(postprob))

for(i in 1:nrow(postprob)){
  
  getprobs = postprob[i,]
  probtopic1[i] = max(getprobs)
  likelytopic1[i] = which(getprobs == probtopic1[i])
  probtopic2[i] = max(getprobs[-likelytopic1[i]])
  likelytopic2[i] = which(getprobs == probtopic2[i])
  
}

topicdf = data.frame(likelytopic1 = likelytopic1,probtopic1 = probtopic1, 
                     likelytopic2 = likelytopic2, probtopic2 = probtopic2)
topicdf$id = as.numeric(row.names(dtm_m2))
topicdf$likelytopic1n = topicnames[topicdf$likelytopic1]

# get number of talks in each topic
topicdf_s = topicdf %>% group_by(likelytopic1n) %>% summarize(numtalks = n())

CairoPNG("topictalks.png")
ggplot(data = topicdf_s) + 
  geom_bar(aes(x = reorder(likelytopic1n,numtalks),y = numtalks),fill = "grey",stat = "identity") + 
  xlab("") + ylab("number of talks") + 
   theme_bw(20) + coord_flip()
dev.off()

# get percentage of talks in a session belonging to each topic (talks are assigned the likely topic)
topicdf2 = inner_join(topicdf,talksdf[,c("id","session")],by = "id")
topicdf2_s = topicdf2 %>% group_by(session,likelytopic1n) %>% summarize(numtalks = n())
topicdf2_s = topicdf2_s %>% group_by(session) %>% mutate(pcttalks = numtalks/sum(numtalks))

CairoPNG("sessiontopics.png",height = 600,width = 1200)
ggplot(data = topicdf2_s) + geom_tile(aes(x = likelytopic1n, y = session, fill = pcttalks)) + 
  scale_fill_gradient(low = "white",high = "darkblue") + 
  xlab("") + ylab("") + guides(fill = FALSE) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

# get percentage of talks in an organization belonging to each topic (talks are assigned likely topic)
# filter to top 10 organizations in terms of talks
topaffil = affildf %>% filter(affil2 != "Remove") %>% group_by(id, affil2) %>% summarize(cnt = n())
topaffil2 = topaffil %>% group_by(affil2) %>% summarize(numtalks = n()) %>% arrange(desc(numtalks))

topicdf2_affil = inner_join(topicdf2,affildf[,c("id","affil2")],by = "id")
topicdf2_affils = topicdf2_affil %>% group_by(affil2,likelytopic1n) %>% summarize(numtalks = n())
topicdf2_affils = topicdf2_affils %>% group_by(affil2) %>% mutate(pcttalks = numtalks/sum(numtalks))
topicdf2_affils_f = inner_join(topicdf2_affils,topaffil2[1:10,c("affil2")],by = "affil2")

CairoPNG("affiltopics.png",height = 600, width = 1200)
ggplot(data = topicdf2_affils_f) + geom_tile(aes(x = likelytopic1n,y = affil2, fill = pcttalks)) + 
  scale_fill_gradient(low = "white",high = "darkblue") + 
  xlab("") + ylab("") + guides(fill = FALSE) + 
  theme_bw(20) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

# Get percentage of talks by acad vs industry affilation
topicdf2_affils2 = inner_join(topicdf2_affils,affillist[,c("affil2","type")],by = "affil2")
topicdf2_affils3 = topicdf2_affils2 %>% group_by(type,likelytopic1n) %>% summarize(numtalks = sum(numtalks)) 
topicdf2_affils3 = topicdf2_affils3 %>% group_by(type) %>% mutate(pcttalks = numtalks/sum(numtalks))

CairoPNG("typetopics.png",height = 600, width = 900)
ggplot(topicdf2_affils3) + 
  geom_bar(aes(x = factor(likelytopic1n),y = pcttalks,fill = type),stat = "identity",position = "dodge") + 
  xlab("") + ylab("% talks") + scale_y_continuous(labels = percent) + 
  theme_bw(20) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()


# Output links to talks for each likely topic

for(j in 1:numtopics){
  topictalks = inner_join(topicdf,talksdf[,c("id","title","talkurl")],by = "id")
  topictalks = topictalks %>% filter(likelytopic1 == j)
  
  outstr = as.character()
  k = 1
  for(i in 1:nrow(topictalks)){
    title = topictalks$title[i]
    talkurl = topictalks$talkurl[i]
    outstr[k] = paste0("[",title,"](",talkurl,")   \n")
    k = k + 1
    #outstr[k] = paste0("Most likely topic ",topictalks$likelytopic1[i], "  Probability ",round(topictalks$probtopic1[i]*100),"%","  ")
    #k = k + 1
    #outstr[k] = paste0("2nd likely topic ",topictalks$likelytopic2[i], "  Probability ",round(topictalks$probtopic2[i]*100),"%","  \n\n")
    #k = k + 1
  }
  cat(paste(outstr,collapse = "\n"),file = paste0("linkFiles/topictalks",j,".Rmd"))
  
}


