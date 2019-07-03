#install.packages(c('tm', 'SnowballC', 'RTextTools', 'plyr'))
setwd("P:\\Trainings\\Competency\\")
library(tm)
library(RTextTools)
library(SnowballC)
library(plyr)

# #assign the rootaddress where all the folders, scripts and files are placed
# rootAddress="P:\\Competency\\"

#update the input file path
data = read.table("Data\\Data.txt", sep = '\t', header = TRUE, quote = "", fill = TRUE)

#Use the tm package we first transfrom the dataset to a corpus:
data=subset(data,data[,2]!="")
data_corpus = Corpus(VectorSource(data[,2]))

#normalize the texts using a series of pre-processing steps: 
#1. Switch to lower case 
#2. Remove numbers 
#3. Remove punctuation marks and stopwords 
#4. Remove extra whitespaces

data_corpus = tm_map(data_corpus, content_transformer(tolower))
data_corpus = tm_map(data_corpus, removeNumbers)
data_corpus = tm_map(data_corpus, removePunctuation)
data_corpus = tm_map(data_corpus, removeWords, c("the", "and", stopwords("english")))
data_corpus =  tm_map(data_corpus, stripWhitespace)



# ngram functions
unigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

#intialize the min and max freq values for DTM
minFreq = min(10,floor(0.001*length(data_corpus)))
maxFreq = 0.8*(length(data_corpus))

#bag of words approach for DTM
unidtm <- DocumentTermMatrix (data_corpus, control=list(wordLengths=c(3,300), 
                                                   tokenize = unigramTokenizer,
                                                   bounds=list(global=c(minFreq,maxFreq)))
);

#inspect(unidtm)
data_dtm=unidtm
data_dtm = removeSparseTerms(data_dtm, 0.99)

# Transform to a matrix.
data_dtm_mat=as.matrix(data_dtm)

# Combine data with labels.
data_dtm_theme <- cbind(Label = data[,3], data.frame(data_dtm_mat))

#sampling the data
percent_train= 0.7 * nrow(data_dtm_theme)
indexes=sample(1:nrow(data_dtm_theme), floor(percent_train))

#assigning training and validating data for model building
testData=data_dtm_theme[-indexes,]
trainData=data_dtm_theme[indexes,]

#training the model
lengthdiv = floor(length(trainData[,1]))
container <- create_container(trainData[,-1],trainData[,1],trainSize=1:length(trainData[,1]), virgin=FALSE)
#container <- create_container(unidtm,macrocode,trainSize=1:length(macrocode[,1]), virgin=FALSE)

#SVM model building
model <- train_models(container, algorithms=c("SVM"))

#save the model
pradd=paste(rootAddress,"Model\\", sep="")
resultsSVM = classify_models(container,model)
save(data_dtm_mat, file = "P:\\Competency\\Model\\SVMMatrix\\Matrix_ngram")
save(model, file = "P:\\Competency\\Model\\SVMModels\\Models_ngram")

#Validating the model
#load the model
load("P:\\Competency\\Model\\SVMMatrix\\Matrix_ngram")
load("P:\\Competency\\Model\\SVMModels\\Models_ngram")

#creating the container for validating the data
container = create_container(testData[,-1], NULL, testSize = 1:length(testData[,1]), virgin = FALSE)
resultsSVM = classify_models(container,model)
#performance(scene.pred.br, measures = list(multilabel.hamloss, multilabel.subset01, multilabel.f1, multilabel.acc))


#combing the predicted values and expected values 
testData_id=rownames(testData)
uniqueid=rownames(data_dtm_theme)
data_dtm_theme=cbind(uniqueid, data_dtm_theme)
resultsSVM=cbind(testData_id,resultsSVM)
colnames(resultsSVM)=c("uniqueid","Predicted.Macrocode","Probability")
data_match=data_dtm_theme[,1:2]

finaldata=join(resultsSVM, data_match, by="uniqueid", type="inner")

pradd = paste(rootAddress,"Temp\\",sep="")

write.table(finaldata, file = paste(pradd,"/","results.tsv",sep=""), row.names = FALSE, sep = "\t")
