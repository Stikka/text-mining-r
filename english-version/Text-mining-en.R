######################################################################################

#Needed <- c("tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc", "topicmodels", "lsa", "scatterplot3d")

#install.packages(Needed, dependencies = TRUE)

#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

#######################################################################################
    
cname <- file.path("C:", "text")   
cname   

dir(cname)

##########################################################################################

library(tm)
docs <- VCorpus(DirSource(cname))
summary(docs)
inspect(docs[1])
writeLines(as.character(docs[1]))

##########################################################################################

docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower)) 

docs <- tm_map(docs, removeWords, stopwords("english"))

docs <- tm_map(docs, stemDocument) 

docs <- tm_map(docs, removeWords, c("may"))

docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[1]))

##########################################################################################
     
dtm <- DocumentTermMatrix(docs)
dtm
tdm <- TermDocumentMatrix(docs)
tdm

##########################################################################################
 
freq <- colSums(as.matrix(tdms))
length(freq)
ord <- order(freq)
   
m <- as.matrix(dtm)   
dim(m)
   
tdm_tfidf <- DocumentTermMatrix(docs, control = list(weighting = weightTfIdf))
tdm_tfidf = removeSparseTerms(tdm_tfidf, 0.5)
tdm_tfidf

inspect(tdm_tfidf[1,1:4]) 
 
dtms <- removeSparseTerms(dtm, 0.4)
dtms  

tdms <- removeSparseTerms(tdm, 0.4)
tdms
 
freq <- colSums(as.matrix(tdms))
freq

head(table(freq), 15) 

tail(table(freq), 15) 

freq <- sort(rowSums(as.matrix(tdms)), decreasing=TRUE)
head(freq, 15)
  
findFreqTerms(dtms, lowfreq=50)

wf <- data.frame(word=names(freq), freq=freq)
head(wf) 

##########################################################################################

library(ggplot2) 

p <- ggplot(subset(wf, freq>45), aes(x = reorder(word, -freq), y = freq), fill = variable) + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1))
p  

library(wordcloud)
     
color <- brewer.pal(6, "Set1")
wordcloud(names(freq), freq, max.words=60, rot.per=0.3, colors=color) 

##########################################################################################

library(lsa)

matrix <- textmatrix("docs/", stemming=TRUE, language="english", stopwords=stopwords_en)
matrix

LSAspace <- lsa(matrix, dims=dimcalc_raw())
LSAspace 
svd(matrix)
round(LSAspace$tk %*% diag(LSAspace$sk) %*% t(LSAspace$dk))
newLSAspace <- lsa(matrix, dims=2)
newMatrix <- round(as.textmatrix(newLSAspace), 2)

associate(matrix, "current")
associate(newMatrix, "bulb")

t.locs <- newLSAspace$tk %*% diag(newLSAspace$sk)
plot(t.locs, type="n")
text(t.locs, labels=rownames(newLSAspace$tk))    

##########################################################################################

library(cluster)

dtms <- removeSparseTerms(dtm, 0.25)
dtms 

d <- dist(t(dtms), method="euclidian")
fit <- hclust(d=d, method="complete")
  
plot.new()
plot(fit, hang=-1) 

groups <- cutree(fit, k=7)  
rect.hclust(fit, k=7, border="red") 
   
library(fpc)
 
dtms <- removeSparseTerms(dtm, 0.25) #matrica za 75% praznog prostora
   
d <- dist(t(dtms), method="euclidian")

kfit <- kmeans(d, 2)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 

##########################################################################################

library(topicmodels)

burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003, 5, 63, 100001, 765)
nstart <- 5
best <- TRUE
 
k <- 6

ldaOut <- LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
 
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics, file = paste("LDAGibbs", k, "Text-theme.txt"))
 
ldaOut.terms <- as.matrix(terms(ldaOut, 6))
write.csv(ldaOut.terms, file = paste("LDAGibbs", k, "Theme-terms.txt"))
 
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities, file = paste("LDAGibbs", k, "Theme-probability.txt"))

