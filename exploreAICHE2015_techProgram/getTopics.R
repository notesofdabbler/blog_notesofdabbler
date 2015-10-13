#
#  Get topics from abstracts of talks
#

library(topicmodels)
library(tm)

# load data
load("talkdetaildf.Rda")

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

termFreq_chk = colSums(dtm_m2)
termFreq_chk = sort(termFreq_chk,decreasing = TRUE)
head(termFreq_chk,30)

rowsum_chk = rowSums(dtm_m2)
dtm_m2 = dtm_m2[rowsum_chk > 0,]

numtopics = 10
SEED = 2345
tmp = LDA(dtm_m2, k = numtopics, control = list(seed = SEED))
Terms = terms(tmp,20)
Terms[,1:10]

postprob = posterior(tmp)$topics

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

