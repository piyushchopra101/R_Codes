###################################################
## A tutorial on performing topic modelling and 
## Query search 
##################################################

install.packages("tm")
install.packages("SnowballC")

#load text mining library
library(tm)
library(SnowballC)

#set working directory (modify path as needed)
setwd('SPECIFY THE PATH TO YOUR DOWNLOADED CORPUS FILES')

#read files into a character vector
medhelp_10000_msg <- read.csv("T:/project/medhelp_10000_msg.csv")

# Create a dataframe sampling 1000 documents. These would be the documents
# we would perform the topic modelling and query on.
files <- as.data.frame(sample(medhelp_10000_msg$text,1000))

# Give the column name to the file.
colnames(files)<-"Text"

# Specify the row names in order to interpret each document easily.
N.docs <- nrow(files)
rownames(files) <- paste0("doc", c(1:N.docs))

# Generate a function which would take up a query vector 
# and perform the search giving out the documents which 
# match best with the query.

myfunc<-function(query) {

  query<-q
  
  # Combine the quey along with the other documents to match its 
  # similarity. 
  x<-rbind(files,(query))
  #create corpus from vector
  my.docs <- DataframeSource(x)
  my.docs$Names <- c(rownames(files), "query")
  
  docs<- Corpus(my.docs)
  
  #inspect a particular document in corpus
  writeLines(as.character(docs[[30]]))
  
  #Start preprocessing
  
  docs<-tm_map(docs, content_transformer(tolower))  
  
  #remove potentially problematic symbols
  toSpace<- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
  docs<- tm_map(docs, toSpace, "-")
  docs <- tm_map( docs, toSpace, ' ' ' )
  docs <- tm_map( docs, toSpace, ' ' ' )
  docs <- tm_map( docs, toSpace, ' . ' )
  docs <- tm_map( docs, toSpace, ' "' )
  docs <- tm_map( docs, toSpace, ' "' )
  
  
  #remove punctuation
  docs <- tm_map(docs, removePunctuation)
  
  #Strip digits
  docs <- tm_map(docs, removeNumbers)
  
  #remove stopwords
  docs <- tm_map(docs, removeWords, stopwords("en"))
  
  #remove whitespace
  docs <- tm_map(docs, stripWhitespace)
  
  #Good practice to check every now and then
  writeLines(as.character(docs[[6]]))
  
  #Stem document
  docs <- tm_map(docs,stemDocument)
  
  #inspect a document as a check
  #writeLines(as.character(docs[[30]]))
  
  
  #Create document-term matrix
  dtm <- DocumentTermMatrix(docs)
  
  #convert rownames to filenames
  rownames(dtm) <- my.docs$Names
  
  #collapse matrix by summing over columns
  freq <- colSums(as.matrix(dtm))
  
  #length should be total number of terms
  length(freq)
  
  #create sort order (descending)
  ord <- order(freq,decreasing=TRUE)
  
  #List all terms in decreasing order of freq and write to disk
  freq[ord][1:5]
  #write.csv(freq[ord],"word_freq.csv")
  
  # Build a matrix to get the total  number of rows and columns.
  dtm_matrix = as.matrix(dtm) 
  top_set_words = sort(colSums(dtm_matrix), decreasing=TRUE) 
  
  # Create a weighted term-frequency Inverse document frequency matrix
  weight<-weightTfIdf(dtm)  
  weight<-as.matrix(weight) 
  
  # Scale the matrix and center the data .
  tfidf.matrix <- scale(weight, center = FALSE, scale = sqrt(colSums(weight^2))) 
  tfidf.matrix[1:5,1:15]
  N.docs<-nrow(files) 
  
  # Extract the query vector which is the last row of the term frequency matrix.
  query.vector <- tfidf.matrix[(N.docs + 1),]  
  tfidf.matrix <- tfidf.matrix[1:500,] 
  
  doc.scores <- (query.vector) %*% t(tfidf.matrix)
  results.df <- data.frame(doc = rownames(files), score = t(doc.scores), text = files$Text) 
  results.df <- results.df[order(results.df$score, decreasing = TRUE),]
  mylist<-list("results.df" = results.df , "top_set_words" = top_set_words,"freq"=freq,"dtm"=dtm,"ord"= ord)
  return(mylist)

} 

##Insert a query string as a vector . The result f1 stores the documents which are most relavent to the search string.
q<-data.frame(Text=" hell anxiety")
f1<-myfunc(q)

#Extract the result 
My_search_result<-f1$results.df

#################################################################
## Extract the terms out of the object f1 for further processing and performing topic modelling
dtm<-f1$dtm
freq<-f1$freq
top_set_words<-data.frame(f1$top_set_words)
ord<-f1$ord

## The results.df stores the sorted document list.
results.df<-f1$results.df

###################################################################
# Create a plot of result

library(ggplot2)

top_set_words =data.frame(Words=rownames(top_set_words),Occurence=top_set_words$f1.top_set_words)
top_set_words$Words = reorder(top_set_words$Words, top_set_words$Occurence)

x11()
ggplot(top_set_words[1:30,], aes(x=Words, y=Occurence)) + geom_point(size=5, colour="red") + coord_flip() +
  ggtitle("Popular Top 30 Words from the documents") + 
  theme(axis.text.x=element_text(size=13,face="bold", colour="black"), axis.text.y=element_text(size=13,colour="black",
                                                                                                face="bold"), axis.title.x=element_text(size=14, face="bold"), axis.title.y=element_text(size=14,face="bold"),
    plot.title=element_text(size=24,face="bold"))

####################################################################
# Lets Start with the Topic Modelling

install.packages("topicmodels")

#load topic models library
library(topicmodels)


#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 5

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#######################

# Plot the words under each topic
install.packages("wordcloud")
library(wordcloud)

topic.top.words <- terms(ldaOut,100)[,1]
topic1<-data.frame(freq[topic.top.words])

x11()
wordcloud(topic.top.words, topic1$freq.topic.top.words., c(4,.8), rot.per=0, random.order=F)

########################
#Inspect the TDM to find the most frequently occurring terms. Start with the words occurred more than 20 times and then, you can try other thresholds as well.
findFreqTerms(dtm,20)

#Now, you are ready to do some analyses over the association of the words. 
findAssocs(dtm,"Your_WORD", 0.75)


######################
#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,6))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)

#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])

#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])
