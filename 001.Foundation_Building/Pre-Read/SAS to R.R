#http://www.statmethods.net/r-tutorial/index.html

### Libname Statement ###

setwd("C:\\Users\\a491441\\Desktop\\Projects\\Segmentation 2.0 Model Building\\Random Forest")
getwd()

# Proc Improt from CSV

Dev_Sample <-read.csv("dev_sample.csv", header=TRUE)

install.packages("xlsx")
library(xlsx)
mydata <- read.xlsx("C:/Users/a491441/Desktop/Projects/Segmentation 2.0 Model Building/test.xlsx", 
                    sheetName = "Sheet1")


# Creating data sets in R

x = 1
x = c(1,2,3,5)
x = 1 : 25
x

# mydata is a data frame. It is like a matrix with three column variables (mydata$x, mydata$n, mydata$y).
# A data frame is like a matrix in which the columns may be of different types 
#(e.g. numerical variable, factor, text). 
#Following is a very brief review of data frame concepts:
#	A data frame is a set of rows and columns.
#	Each row is of the same length and data type
#	Every column is of the same length but can be of differing data types
#	A data frame has characteristics of both a matrix and a list
#	Bracket notation is the customary method of indexing into a data frame

#### Creating a data set with multiple obs. and more than 1 variables ####

mydata <- data.frame(x = 20, n = 50, y = 6)
mydata <- data.frame(x = c(20,35,45,55,70), n = rep(50,5), y = c(6,17,26,37,44))
mydata

#### Creating a data set with a character variable ####

mydata <- data.frame(x = 20, name = 'Jack', y = 6)
mydata

#### making an exact copy of a data set - data set run ####

mydata <- data.frame(x = c(20,35,45,55,70), n = rep(50,5), y = c(6,17,26,37,44))
newdata <- mydata
newdata

#### creating a new variable and re-labelling it - Createing Indicator#

#Operator	Description
#+	addition
#-	subtraction
#*	multiplication
#/	division
#^ or **	exponentiation
#x %% y	modulus (x mod y) 5%%2 is 1
#x %/% y	integer division 5%/%2 is 2


#Operator	Description
# <	less than
# <=	less than or equal to
# >	greater than
# >=	greater than or equal to
# ==	exactly equal to
# !=	not equal to
# !x	Not x
# x | y	x OR y
# x & y	x AND y
# isTRUE(x)	test if X is TRUE

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44))
mydata

mydata$Ind1 = (mydata$x * mydata$y)
mydata$Ind2 = (mydata$x / mydata$y)
mydata$Ind3 = (mydata$x + mydata$y) + 10
mydata$Ind4 = (mydata$x - mydata$y) * 2
mydata

#Built-in Functions****************************************************

# abs(x)	absolute value
# sqrt(x)	square root
# ceiling(x)	ceiling(3.475) is 4
# floor(x)	floor(3.475) is 3
# trunc(x)	trunc(5.99) is 5
# round(x, digits=n)	round(3.475, digits=2) is 3.48
# signif(x, digits=n)	signif(3.475, digits=2) is 3.5
# cos(x), sin(x), tan(x)	also acos(x), cosh(x), acosh(x), etc.
# log(x)	natural logarithm
# log10(x)	common logarithm
# exp(x)	e^x


# substr(x, start=n1, stop=n2): Extract or replace substrings in a character vector.
x <- "abcdef" 
substr(x, 2, 4)
substr(x, 2, 4) <- "22222" 


#grep(pattern, x , ignore.case=FALSE, fixed=FALSE)
# Search for pattern in x. 
# If fixed =FALSE then pattern is a regular expression. 
# If fixed=TRUE then pattern is a text string. Returns matching indices.
# grep("A", c("b","A","c"), fixed=TRUE) returns 2


#sub(pattern, replacement, x, ignore.case =FALSE, fixed=FALSE)
# Find pattern in x and replace with replacement text. 
# If fixed=FALSE then pattern is a regular expression.
# If fixed = T then pattern is a text string. 
# sub("\\s",".","Hello There") returns "Hello.There"

#strsplit(x, split)
#Split the elements of character vector x at split. 
#strsplit("abc", "") returns 3 element vector "a","b","c"

#paste(..., sep="")
#Concatenate strings after using sep string to seperate them.
# paste("x",1:3,sep="") returns c("x1","x2" "x3")
# paste("x",1:3,sep="M") returns c("xM1","xM2" "xM3")
# paste("Today is", date())

#toupper(x)	Uppercase
#tolower(x)	Lowercase


#dnorm(x)
#normal density function (by default m=0 sd=1)
  # plot standard normal curve
  x <- pretty(c(-3,3), 30)
y <- dnorm(x)
plot(x, y, type='l', xlab="Normal Deviate", ylab="Density", yaxs="i")



#Re-labelling the Column - Rename and label Statement####

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44))
mydata
colnames(mydata)[2] <- "New_Var"
mydata

#Value Labeling
mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(1,2,3,1,2))
mydata$v1 <- factor(mydata$z,
                    levels = c(1,2,3),
                    labels = c("red", "blue", "green"))
mydata

#proc print Statement

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(12,34,56,23,49))
View(head(mydata))
mydata


#Proc print with obs
mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(12,34,56,23,49))
View(head(mydata))
head(mydata, n=2)
tail(mydata, n=3)

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(12,34,56,23,49))
newdata <- mydata[1:2,]
newdata

#checking dataset

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(1,2,3,1,2))
mydata$factorvar <- factor(mydata$z, levels = c(1,2,3))

ls(mydata)
names(mydata)
str(mydata)
levels(mydata$factorvar)
dim(mydata)
class(mydata)
class(x)
class(mydata$factorvar)

#proc contents Statement
mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(12,34,56,23,49))
str(mydata)
dim(mydata)
colnames(mydata)

#Variable Type Conversions in R*****************************

#Numeric to factor

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(1,2,3,4,5))
mydata$factorvar <- factor(mydata$z, levels = c(1,2,3,4,5))

#Numeric to character

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(1,2,3,4,5))
mydata$charvar <- as.character(mydata$z)
mydata

#Character to numeric

mydata$numvar <- as.numeric(mydata$charvar)
mydata
str(mydata)

#Playing with date Conditions****************************************

#%d	day as a number (0-31)	01-31
#%A unabbreviated weekday	Monday
#%m	month (00-12)	00-12
#%B unabbreviated month	January
#%Y 4 digit Year 2007

today <- Sys.Date()
format(today, format="%B %d %Y")

# convert date info in format 'mm/dd/yyyy'

strDates <- c("01/05/1965", "08/16/1975")
dates <- as.Date(strDates, "%m/%d/%Y")
dates

mydates <- as.Date(c("2007-06-22", "2004-02-13"))
mydates

#### dropping or keeping vars ####********************************

#Drop*****
mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(1,2,3,4,5))
newdata = subset(mydata, select = -c(x,y) )
newdata          

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(1,2,3,4,5))
newdata = mydata[ -c(1,3) ]   
newdata           
            
#Keep*****
mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(1,2,3,4,5))
newdata = subset(mydata, select = c(x,y) )
newdata   

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(1,2,3,4,5))
newdata = mydata[ c(1,2) ]   
newdata 

#****Substring****
mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(121,4562,3643,44569,5876))
mydata$charvar <- as.character(mydata$z)
mydata$charvar1 <- substr(mydata$charvar,1,2)
mydata

#****Replace****

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(121234,4562578,3643345,44569345,5876987))
mydata$charvar <- as.character(mydata$z)
substr(mydata$charvar, 2, 3) <- "A"
mydata

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(121234,4562578,3643345,44569345,5876987))
mydata$charvar <- as.character(mydata$z)
substr(mydata$charvar, 2, 4) <- "TIF"
mydata

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(121234,4562578,3643345,44569345,5876987))
mydata$charvar <- as.character(mydata$z)
substr(mydata$charvar, 5, 8) <- "ABCD"
mydata

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(121234,4562578,3643345,44569345,5876987))
mydata$charvar <- as.character(mydata$z)
substring(mydata$charvar, 5) <- "Rahul"
mydata


#Where or Subsetting**********
mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(1,2,3,4,5))
subset <- mydata[ which(mydata$x > 50),]
subset

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(1,2,3,4,5))
mydata$charvar <- as.character(mydata$z)
mydata
str(mydata)
subset_data <- mydata[(mydata$charvar)  == '1','2']
subset_data

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(1,2,3,2,3))
mydata$charvar <- as.character(mydata$z)
newdata <- mydata[ which(mydata$charvar=='2' & mydata$x > 50), ]
newdata

mydata <- data.frame(x = c(20,35,45,55,70), y = c(6,17,26,37,44), z = c(1,2,3,2,3))
mydata$charvar <- as.character(mydata$z)
newdata <- mydata[ which(mydata$charvar=='2' | mydata$x > 50), ]
newdata

#Random Sample without replacement
mysample <- Dev_Sample[sample(1:nrow(Dev_Sample), 50,replace=FALSE)]
mysample

#Random Sample with replacement
mysample <- Dev_Sample[sample(1:nrow(Dev_Sample), 50,replace=TRUE)]
mysample


############# RESHAPING DATA ###############

## TRANSPOSE
t(mydata)

## Reshape package
# Basically, the package "melt" data so that each row is a unique id-variable combination. 
# Then it "cast" the melted data into any shape we would like.

# example of melt function 
library(reshape)
mydata <- melt(mydata, id=c("x","y"))

# cast the melted data
# cast(data, formula, function) 
xjmeans <- cast(mydata,x~variable, mean)
ymeans <- cast(mydata, y~variable, mean)


############### Data Type Conversion #################

# From				To					Command
# vector			one long vector		c(x,y)
# vector			matrix				cbind(x,y) or rbind(x,y)
# vector			data frame			data.frame(x,y)
# matrix			one long vector		as.vector(mymatrix)
# matrix			matrix				
# matrix			data frame			as.data.frame(x,y)
# data frame		one long vector		
# data frame		matrix				as.matrix(myframe)
# data frame		data frame			

############## AGGREGATING DATA ######################

# aggregate data frame mtcars by cyl and vs, returning means
# for numeric variables
attach(mtcars)
aggdata <-aggregate(mtcars, by=list(cyl,vs), 
  FUN=mean, na.rm=TRUE)
print(aggdata)
detach(mtcars)

############# SORTING AND MERGING DATA ###############

# sorting examples using the mtcars dataset
attach(mtcars)
#sort by mpg (ascending) and cyl (descending)
newdata <- mtcars[order(mpg, -cyl),] 

# Adding Columns
# merge two data frames by ID
total <- merge(data frameA,data frameB,by="ID")
# merge two data frames by ID and Country
total <- merge(data frameA,data frameB,by=c("ID","Country"))

#Adding Rows
total <- rbind(data frameA, data frameB)
# To join two data frames (datasets) vertically, use the rbind function. 
# The two data frames must have the same variables, but they do not have 
# to be in the same order.


############ Control Structures #####################

#if-else
ifelse(test condition,yes,no)

#for
x = NA
for (i in 1:100)
{
  x[i] = i+25
}
x










