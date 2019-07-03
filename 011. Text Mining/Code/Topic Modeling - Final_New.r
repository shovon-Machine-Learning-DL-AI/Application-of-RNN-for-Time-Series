#Install and load the libraries
library("tm");
library("topicmodels")
library(slam)

#The number of topics and terms for each topics to be initialized
noTopics = 7
noTerms = 5
minimumWordFreq = 3

#The input file path
data=read.table("Data\\Data.txt", sep = '\t', header = TRUE, quote = "", fill = TRUE)

#The output file path
outputFile = "Output\\filename.tsv"

#Create a corpus for further processing
corpus <- Corpus(VectorSource(as.matrix(data[,"DRIVER_VERBATIM"]))); 

#normalize the texts using a series of pre-processing steps: 
#1. Switch to lower case 
#2. Remove punctuation marks and stopwords 
#3. Remove extra whitespaces
corpus <- tm_map(corpus, stripWhitespace);
corpus <- tm_map(corpus, tolower);
corpus <- tm_map(corpus, removePunctuation);
corpus <- tm_map(corpus, removeWords, stopwords("english"));

# Perform stemming on input corpus
corpus <- tm_map (corpus, stemDocument);

#Create a document term matrix
dtminter <- DocumentTermMatrix (corpus, control=list(wordLengths=c(3,300), 
                                                     bounds=list(global=c(minimumWordFreq,0.8*(length(corpus))))));

# sum words in each document
rowCount <- apply (dtminter,1,sum) 
dtminter <- as.DocumentTermMatrix (dtminter[(rowCount)>0, ]);

#CTM(dtm,NoOfClusters)
ctmObj <- CTM(dtminter,noTopics,method="VEM", control=NULL, model=NULL);

#Combining Results
clusterInfo <- matrix(data="",nrow=length(as.matrix(topics(ctmObj, 1))),ncol=(noTerms+1));
row.names(clusterInfo) <- (row.names(as.matrix(topics(ctmObj, 1)))); 
clusterInfo[,1] <- as.matrix(topics(ctmObj, 1))

#No of Topics getting Appended to the File
topicTerms = as.matrix(terms(ctmObj,noTerms))

#The cluster terms
for(j in 1:length(clusterInfo[,1])){
  clusterNo = as.numeric(clusterInfo[j,1])
  clusterInfo[j,2:(noTerms+1)] = t(topicTerms[,clusterNo])
  
}
colnames(clusterInfo) = c("Cluster ID","Topic-1","Topic-2","Topic-3","Topic-4","Topic-5")

#Merge data and results
inputData = merge(data,  clusterInfo, by = 0, all.x = TRUE)

#Write output file
write.table(inputData, file = outputFile , append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
