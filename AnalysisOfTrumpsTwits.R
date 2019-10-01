
#list packages i.e required for this project
Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
            "cluster", "igraph", "fpc")
install.packages(Needed, dpendencies = True)

#extract text and metadata from PDF documents
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

path <- file.path("C:/Users/family/Desktop/Assignments/ALY 6040/week1","texts")
dir(path)

library(tm)

#Create Corpus of all text file
docs <- VCorpus(DirSource(path))   
summary(docs)   

inspect(docs[1])


#Data preprocessing

#remove punctuation
docs <- tm_map(docs,removePunctuation) 

#Replce the special character with space
for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])
  docs[[j]] <- gsub("http://(\\S+)|\\s*[\\^w\\s]\\s*[^c#c++\\s]","",docs[[j]])
}

#Remove numbers  
docs <- tm_map(docs, removeNumbers)  
#make lower case 
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs # Make a copy of processed data.

#removing stop words
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, PlainTextDocument)
#Removing particular words
docs <- tm_map(docs, removeWords, c("syllogism", "tautology"))   

#Keep few words together, because they make sence together
for (j in seq(docs))
{
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}

docs <- tm_map(docs, PlainTextDocument)
docs_st <- tm_map(docs, stemDocument)   
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1]))

docs_stc <- tm_map(docs_st, stemCompletion, dictionary = DocsCopy, lazy=TRUE)
docs_stc <- tm_map(docs_stc, PlainTextDocument)
writeLines(as.character(docs_stc[1]))

docs <- tm_map(docs, stripWhitespace)

docs <- tm_map(docs, PlainTextDocument)

#get the frequency of each words for every documents
dtm <- DocumentTermMatrix(docs)   
dtm 

#tdm <- TermDocumentMatrix(docs)   
#tdm  


#arrange them by frequency
freq <- colSums(as.matrix(dtm))   
length(freq)

ord <- order(freq) 

m <- as.matrix(dtm) 
dim(m) 

#save file as csv for future work
write.csv(m, file="DocumentTermMatrix.csv")

#sparse the matric by 20%, which it empty the space by 20% max.
dtms <- removeSparseTerms(dtm, 0.2)   
dtms

#analyse the frequencies (min and max)
freq <- colSums(as.matrix(dtm))
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)  
head(freq,14)
head(table(freq), 20) 
tail(table(freq), 20) 

freq <- colSums(as.matrix(dtms))   
freq 

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14) #select top 14

#create a data frame
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)

#Visualization for Analysis

#Word Frequency
library(ggplot2)#load package ggplot2
#Plot a histogram for words that appear at least 50 times
p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#Calculate terms correlations
#Find the words which highly correlated with foolowing words
#I keep set limit to 75%
findAssocs(dtm, c("country" , "american"), corlimit=0.75) 

#Create word clouds#
library(wordcloud)   
dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=150,rot.per=0.2, colors=dark2)    


#Hierarchal Clustering#####   
dtms <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space.
library(cluster)   
d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
fit <- hclust(d=d, method="complete")    # Also try: method="ward.D"   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=6)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=6, border="red") # draw dendogram with red borders around the 5 clusters   



#K-means clustering#  
library(fpc)   
library(cluster)  
dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)   
d <- dist(t(dtms), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  



