#Install the required packages
install.packages(arules) -- #to use the Apriori function for Association rules
install.packages(arulesViz) -- #to be used for visualization of the data/results etc.
install.packages(xlsx) -- #to Export the results to an Excel

#Load the Packages  
library(arules)
library(arulesViz)
library(xlsx)
  
#Set your R to the working Directory to read/write the files by  
setwd("C:\\Users\\A604893\\Documents\\MBA")
  
#Load the data
Data = read.transactions("SampleData.csv", format = "single", sep=",", cols=c(1,2))

#Inspect the data
summary(Data)

inspect(Data[1:5])

itemFrequencyPlot(Data,topN=20,type="absolute")

#Run the Algorithm for defining the rules
Rules <- apriori(Data, parameter = list(supp = 0.007, conf = 0.01,maxlen = 2))

##Supp ??? used for defining the cutoff of the Support function for the rules (Range 0 - 1)

##Conf ??? used for defining the cutoff of the Confidence function for the rules

##maxlen ??? used for defining the Length of the Association Rules being formed (Min. 2)

# Inspect the Rules
inspect(Rules[1:5])

summary(Rules)

Rules <- sort(Rules, by="confidence", decreasing=TRUE)
##sort ??? used for sorting the results by "confidence"/ "Support" etc.

inspect(Rules[1:5]) #inspect rules to check if the results are sorted & get the top 5 rules

#Write the Results to table and Export to Excel

Results <- as(Rules, "data.frame") ##as function is used to convert the data from one form to the other

write.xlsx(Results, "Results.xlsx") ##write.xlsx function is used to export the results in an Excel sheet to the working directory