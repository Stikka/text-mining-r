######################################################################################

#PRVI KORAK: Paketi u R potrebni da bi zapoceli sa rudarenjem teksta; jednom se ucitavaju

#Needed <- c("tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc", "topicmodels", "lsa", "scatterplot3d")

#install.packages(Needed, dependencies = TRUE)

#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

#######################################################################################

#DRUGI KORAK: Ucitavanje teksta

#Zapocinje tako da tekstualne datoteke spremimo u mapu imena "tekst".
#To ce biti nas "corpus", odnosno tijelo tekstova koje cemo rudariti.
#Spremit cemo mapu na nas C disk koristeci sljedece naredbe.  
    
cname <- file.path("C:", "tekst")   
cname   

dir(cname) #naredba kojom mozemo vidjeti jesu li se dokumenti ucitali

##########################################################################################

#TRECI KORAK: Pocetak analize

library(tm) #ucitavanje R paketa za rudarenje teksta
docs <- VCorpus(DirSource(cname)) #ucitavanje tekstova u R
summary(docs) #lista svih tekstualnih datoteka unutar mape
inspect(docs[1]) #detalji o prvoj datoteci unutar corpusa, poput broja znakova
writeLines(as.character(docs[1])) #ako zelimo ucitati tekst prve datoteke na ekranu

##########################################################################################

#CETVRTI KORAK: Pretprocesiranje

docs <- tm_map(docs, removePunctuation) #uklanjanje interpunkcijskih znakova
docs <- tm_map(docs, removeNumbers) #uklanjanje brojeva

docs <- tm_map(docs, stripWhitespace) #brisanje praznog prostora u dokumentima
docs <- tm_map(docs, content_transformer(tolower)) #sva slova neka budu mala

docs <- tm_map(docs, removeWords, stopwords("english")) #uklanjanje rijeci koje nemaju nikakvo bitno znacanje; engleske rijeci poput a, and, if, the itd.

docs <- tm_map(docs, stemDocument) #"stemming", odnosno kracanje rijeci; pr. iz rijeci "cleaning" izbacujemo nastavak "ing" i dobivamo novu rijec clean 

docs <- tm_map(docs, removeWords, c("may"))  #uklanjamo rijec "may"

docs <- tm_map(docs, PlainTextDocument) #R ce pretprocesirane datoteke smatrati teksutalnim datotekama
writeLines(as.character(docs[1]))

##########################################################################################

#PETI KORAK: Prikaz podataka
     
dtm <- DocumentTermMatrix(docs) #matrica koja nam opisuje ucestalost pojmova koji se pojavljuju u zbirci dokumenata
dtm
tdm <- TermDocumentMatrix(docs) #transponirana matrica
tdm

##########################################################################################

#SESTI KORAK: Bag-of-words model, term weighting scheme
 
freq <- colSums(as.matrix(tdms)) #ustroj pojmova prema ucestalosti
length(freq)
ord <- order(freq)
   
m <- as.matrix(dtm)   
dim(m) #vraca nam broj redova i broj stupaca matrice dtm
   
tdm_tfidf <- DocumentTermMatrix(docs, control = list(weighting = weightTfIdf)) #tf-idf vektorizacija
tdm_tfidf = removeSparseTerms(tdm_tfidf, 0.5)
tdm_tfidf

inspect(tdm_tfidf[1,1:4]) # vjerojatnosti pojave prva 4 pojma u prvom dokumentu
 
dtms <- removeSparseTerms(dtm, 0.4) #uklanjaju se rijeci koje se ne pojavljuju cesto u corpusu na nacin da ce matrica imati 60% praznog prostora
dtms  

tdms <- removeSparseTerms(tdm, 0.4)
tdms
 
freq <- colSums(as.matrix(tdms)) #ustroj pojmova prema ucestalosti nakon sto smo uklonili rijeci koje se ne pojavljuju cesto
freq

head(table(freq), 15) #daje nam raspodjelu pojmova koji se najrjede koriste; broj 15 nam kaze da zelimo vidjeti samo prvih 15 frekvencija

tail(table(freq), 15) #daje nam raspodjelu pojmova koji se najcesce koriste; broj 20 nam kaze da zelimo vidjeti samo zadnjih 20 frekvencija

freq <- sort(rowSums(as.matrix(tdms)), decreasing=TRUE) #naredba slicna prijasnjoj koja ureduje popis pojmova prema njihovoj ucestalosti
head(freq, 15)
  
findFreqTerms(dtms, lowfreq=50) #popis pojmova koji se pojavljuju vise od 50 puta

wf <- data.frame(word=names(freq), freq=freq) #drugi nacin kako prikazati ucestalost pojmova
head(wf) 

##########################################################################################

#SEDMI KORAK: Graf ucestalosti pojmova, oblaci  

library(ggplot2) #ucitavanje R paketa za crtanje grafova   

p <- ggplot(subset(wf, freq>45), aes(x = reorder(word, -freq), y = freq), fill = variable) + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1))
p  

library(wordcloud) #ucitavanje R paketa za prikaz pojmova u obliku oblaka
     
color <- brewer.pal(6, "Set1") #ucitavanje paleta boja kojima ce oblak biti prikazan
wordcloud(names(freq), freq, max.words=60, rot.per=0.3, colors=color) 

##########################################################################################

#OSMI KORAK: LSA, veze medu pojmovima (korelacija)

library(lsa) #ucitavanje R paketa za LSA

matrix <- textmatrix("docs/", stemming=TRUE, language="english", stopwords=stopwords_en)
matrix

LSAspace <- lsa(matrix, dims=dimcalc_raw())
LSAspace 
svd(matrix)
round(LSAspace$tk %*% diag(LSAspace$sk) %*% t(LSAspace$dk))
newLSAspace <- lsa(matrix, dims=2)
newMatrix <- round(as.textmatrix(newLSAspace), 2)

associate(matrix, "current") #korelacija izmedu pojmova, jednaka je 1 ako su korelirani
associate(newMatrix, "bulb")

t.locs <- newLSAspace$tk %*% diag(newLSAspace$sk)
#t.locs #pripaziti - brzo popuni ekran
plot(t.locs, type="n")
text(t.locs, labels=rownames(newLSAspace$tk))    

##########################################################################################

#DEVETI KORAK: Klasteriranje

#Hijerarhijsko klasteriranje (HCA)

library(cluster) #ucitavanje R paketa za klasteriranje

dtms <- removeSparseTerms(dtm, 0.25) #uklanjaju se rijeci koje se ne pojavljuju cesto u corpusu na nacin da ce matrica imati 75% praznog prostora
dtms 

d <- dist(t(dtms), method="euclidian") #mjeri se najmanja udaljenost izmedu dva pojma
fit <- hclust(d=d, method="complete") #hijerarhijsko klasteriranje pomocu razlicitih metoda(single, complete, average itd.) 
  
plot.new()
plot(fit, hang=-1) #dendrogram (evolucijsko stablo)

groups <- cutree(fit, k=7) #k je broj klastera koje koristimo   
rect.hclust(fit, k=7, border="red") #dendrogram sa 7 klastera obuhvacenih crvenom bojom   

#Klasterizacija metodom K-srednjih vrijednosti (K-means clustering)
   
library(fpc) #ucitavanje R paketa razlicitih metoda za klasteriranje
 
dtms <- removeSparseTerms(dtm, 0.25) #matrica za 75% praznog prostora
   
d <- dist(t(dtms), method="euclidian") #mjeri se najmanja udaljenost izmedu dva pojma
   
kfit <- kmeans(d, 2) #klasteriranje pojmova u odreden broj grupa(u nasem slucaju 2) te se trazi njihovo srediste
#podaci se predaju klasteru cije je srediste najblize te se racuna novo srediste za svaki klaster
#unutarnja varijacija klastera izracunava se kao zbroj euklidske udaljenosti izmedu tocaka pojmova i klaster sredista.

clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 

##########################################################################################

#DOPUNA - OSMI KORAK: Latentna Dirichlet raspodjela (Latent Dirichlet allocation (LDA))

library(topicmodels) #ucitavanje R paketa razlicitih metoda za klasteriranje
 
#postavljanje parametara za Gibbsovo uzimanje uzorka (Gibbs sampling)
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003, 5, 63, 100001, 765)
nstart <- 5
best <- TRUE
 
k <- 6 #broj tema

ldaOut <- LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
 
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics, file = paste("LDAGibbs", k, "Tekstovi-teme.txt")) #rezultat: tekst u teme
 
ldaOut.terms <- as.matrix(terms(ldaOut, 6))
write.csv(ldaOut.terms, file = paste("LDAGibbs", k, "Teme-pojmovi.txt")) #rezultat: teme u pojmove(6)
 
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities, file = paste("LDAGibbs", k, "Teme-vjerojatnost.txt")) #vjerojatnosti pridruzene za svaku temu

