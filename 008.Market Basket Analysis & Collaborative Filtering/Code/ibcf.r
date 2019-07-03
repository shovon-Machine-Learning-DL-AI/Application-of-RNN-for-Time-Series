
# Set data path as per your data file 
setwd("C:\\Users\\A604893\\Documents\\CF")

# If not installed, first install following three packages in R
library(recommenderlab)
library(reshape2)
library(ggplot2)

# Read data file along with header
data<-read.csv("Training.csv",header=TRUE)

#to divide data into tarin and test
smp_size <- floor(0.8 * nrow(data))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

tr <- data[train_ind, ] #training data
te1 <- data[-train_ind, ] #test data

# Just look at first few lines of this file
head(tr)
# Remove 'id' column. We do not need it
tr<-tr[,-c(1)]

# Using acast to convert above data as follows:
#       m1  m2   m3   m4
# u1    3   4    2    5
# u2    1   6    5
# u3    4   4    2    5
g<-acast(tr, user ~ movie)
# Check the class of g
class(g)


# Convert it as a matrix
R<-as.matrix(g)
# Convert R into realRatingMatrix data structure
#   realRatingMatrix is a recommenderlab sparse-matrix like data-structure
r <- as(R, "realRatingMatrix")
r


# Create a recommender object (model)
#   Run anyone of the following four code lines.
#     Do not run all four
#       They pertain to four different algorithms.
#        UBCF: User-based collaborative filtering
#        IBCF: Item-based collaborative filtering
#      Parameter 'method' decides similarity measure
#        Cosine or Jaccard

# Start the clock!
ptm <- proc.time()
#rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
#rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
rec=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Jaccard",minRating=1))
#rec=Recommender(r[1:nrow(r)],method="POPULAR")

# Stop the clock
proc.time() - ptm
#user  system elapsed 
#1080.70  128.34 1219.91 

# Depending upon your selection, examine what you got
names(getModel(rec))


############Create predictions#############################
# This prediction does not predict movie ratings for test.
#   But it fills up the user 'X' item matrix so that
#    for any userid and movieid, I can find predicted rating
#     dim(r) shows there are 6040 users (rows)
#      'type' parameter decides whether you want ratings or top-n items
#         get top-10 recommendations for a user, as:
#             predict(rec, r[1:nrow(r)], type="topNList", n=10)
recom <- predict(rec, r[1:nrow(r)], type="ratings")
recom

# Get ratings list
rec_list<-as(recom,"list")
head(summary(rec_list))

#on the test data
te<-te1[,-c(4)] #drop the ratings column

ratings<-NULL
# For all lines in test file, one by one
for ( u in 1:nrow(te))
{
  # Read userid and movieid from columns 2 and 3 of test data
  userid <- as.integer(te[u,2])
  movieid<-as.integer(te[u,3])
  
  # Get as list & then convert to data frame all recommendations for user: userid
  u1<-as.data.frame(rec_list[[userid]])
  # Create a (second column) column-id in the data-frame u1 and populate it with row-names
  # Remember (or check) that rownames of u1 contain are by movie-ids
  # We use row.names() function
  u1$id<-row.names(u1)
  u1$id<-as.integer(u1$id)
  # Now access movie ratings in column 1 of u1
  x= u1[u1$id==movieid,1]
  # print(u)
  # print(length(x))
  # If no ratings were found, assign 0. You could also
  #   assign user-average
  if (length(x)==0){
    ratings[u] <- 0
  }else {
    ratings[u] <-x
  }
  
}
length(ratings)
tx<-cbind(te1,round(ratings))
write.csv(tx,"result_IBCF.csv", row.names = F)

#to check accuracy
tx$error<-(tx$rating-tx$`round(ratings)`)

# Function that returns Root Mean Square Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}
# Function that returns Mean Square Error
mse <- function(error)
{
  (mean(error^2))
}
# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}
rmse(tx$error)
mse(tx$error)
mae(tx$error)