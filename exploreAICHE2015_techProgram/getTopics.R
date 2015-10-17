#
#  Get topics from abstracts of talks
#

library(topicmodels)
library(tm)
library(dplyr)
library(tidyr)
library(ggplot2)

# load data
load(file = "talksdf.Rda") # talks
load("talkdetaildf.Rda") # talk abstracts
affildf = read.csv("affildf_clean.csv",stringsAsFactors = FALSE) # list of organizations for each talk


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

termFreq_chk = colSums(dtm_m2)
termFreq_chk = sort(termFreq_chk,decreasing = TRUE)
head(termFreq_chk,30)

rowsum_chk = rowSums(dtm_m2)
dtm_m2 = dtm_m2[rowsum_chk > 0,]
dim(dtm_m2)

numtopics = 10
SEED = 2345
fittopic = LDA(dtm_m2, k = numtopics, control = list(seed = SEED))
Terms = terms(fittopic,10)
Terms[,1:numtopics]

topicnames = c("bio","crystallization","process design","HME","crystallization 2","dissolution","formulation",
               "modeling","granulation","continuous")
names(topicnames) = paste0("X",seq(1,10))

postprob = posterior(fittopic)$topics


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

dfpostprob = data.frame(postprob)
dfpostprob$id = as.numeric(row.names(dtm_m2))
dfpostprob_g = gather(dfpostprob,topicnum,prob,-id)
dfpostprob_g = inner_join(dfpostprob_g,talksdf[,c("id","session")],by = "id")

dfpostprob_gsumm = dfpostprob_g %>% group_by(session,topicnum) %>% summarize(avgprob = mean(prob))
dfpostprob_gsumm$topicname = topicnames[dfpostprob_gsumm$topicnum]

dfpostprob_summ = spread(dfpostprob_gsumm,topicnum,avgprob)


likelytopic1s = rep(0,nrow(dfpostprob_summ))
probtopic1s = rep(0,nrow(dfpostprob_summ))
likelytopic2s = rep(0,nrow(dfpostprob_summ))
probtopic2s = rep(0,nrow(dfpostprob_summ))

for(i in 1:nrow(dfpostprob_summ)){

  print(i)  
  getprobs = as.numeric(dfpostprob_summ[i,2:11])
  probtopic1s[i] = max(getprobs)
  likelytopic1s[i] = which(getprobs == probtopic1s[i])
  probtopic2s[i] = max(getprobs[-likelytopic1s[i]])
  likelytopic2s[i] = which(getprobs == probtopic2s[i])
  
}

sessiontopics = data.frame(dfpostprob_summ$session, likelytopic1s = likelytopic1s,probtopic1s = probtopic1s, 
                                     likelytopic2s = likelytopic2s, probtopic2s = probtopic2s)
sessiontopics$top2prob = sessiontopics$probtopic1s + sessiontopics$probtopic2s

ggplot(data = dfpostprob_gsumm) + geom_tile(aes(x = topicname, y = session, fill = avgprob)) + 
  scale_fill_gradient(low = "white",high = "darkblue") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
dfpostprob_gaffil = inner_join(dfpostprob_g,affildf[,c("id","affil2")],by = "id")
dfpostprob_gaffilsumm = dfpostprob_gaffil %>% group_by(affil2,topicnum) %>% summarize(avgprob = mean(prob))
dfpostprob_affilsumm = spread(dfpostprob_gaffilsumm,topicnum,avgprob)

topaffil = affildf %>% filter(affil2 != "Remove") %>% group_by(id, affil2) %>% summarize(cnt = n())
topaffil2 = topaffil %>% group_by(affil2) %>% summarize(numtalks = n()) %>% arrange(desc(numtalks))

dfpostprob_gaffilsumm2 = inner_join(dfpostprob_gaffilsumm,topaffil2[1:10,1],by = "affil2")

ggplot(data = dfpostprob_gaffilsumm2) + geom_tile(aes(x = topicnum,y = affil2, fill = avgprob)) + 
    scale_fill_gradient(low = "white",high = "darkblue") + theme_bw()

# Output links to talks for each likely topic

for(j in 1:numtopics){
  topictalks = inner_join(topicdf,talksdf[,c("id","title","talkurl")],by = "id")
  topictalks = topictalks %>% filter(likelytopic1 == j)
  
  outstr = as.character()
  k = 1
  for(i in 1:nrow(topictalks)){
    title = topictalks$title[i]
    talkurl = topictalks$talkurl[i]
    outstr[k] = paste0("[",title,"](",talkurl,")   ")
    k = k + 1
    outstr[k] = paste0("Most likely topic ",topictalks$likelytopic1[i], "  Probability ",round(topictalks$probtopic1[i]*100),"%","  ")
    k = k + 1
    outstr[k] = paste0("2nd likely topic ",topictalks$likelytopic2[i], "  Probability ",round(topictalks$probtopic2[i]*100),"%","  \n\n")
    k = k + 1
  }
  cat(paste(outstr,collapse = "\n"),file = paste0("linkFiles/topictalks",j,".Rmd"))
  
}
