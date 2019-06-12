#######################################
## Cassandra Postma
## DS MRP - Exploratory Analysis
## Due June 17th
#######################################

install.packages("corrplot")
install.packages("dplyr")
library("corrplot")
library("ggplot2")
library("dplyr")


#######################################
##Read data files
#######################################
movie_titles <- read.delim("C:\\Users\\MrW\\Desktop\\Cassy's School Stuff\\MRP\\movie_titles.csv", header=FALSE, sep = ",", fill = TRUE, check.names = FALSE)
colnames(movie_titles)<- c("MovieID", "Year", "Movie")
nrow(movie_titles)

##1 Dinosaur Planet
train1 <- read.table("C:\\Users\\MrW\\Desktop\\Cassy's School Stuff\\MRP\\combined_data_1.txt", header=TRUE,sep = ",", skip = 1, fill = TRUE)
colnames(train1)<- c("CustomerID", "Rating", "Date")

##4500 Les Dames du Bois de Boulogne 
train2 <- read.table("C:\\Users\\MrW\\Desktop\\Cassy's School Stuff\\MRP\\combined_data_2.txt", header=TRUE,sep = ",", skip = 1, fill = TRUE)
colnames(train2)<- c("CustomerID", "Rating", "Date")

##9211 Blue Juice 
train3 <- read.table("C:\\Users\\MrW\\Desktop\\Cassy's School Stuff\\MRP\\combined_data_3.txt", header=TRUE,sep = ",", skip = 1, fill = TRUE)
colnames(train3)<- c("CustomerID", "Rating", "Date")

##13368 Sarfarosh 
train4 <- read.table("C:\\Users\\MrW\\Desktop\\Cassy's School Stuff\\MRP\\combined_data_4.txt", header=TRUE,sep = ",", skip = 1, fill = TRUE)
colnames(train4)<- c("CustomerID", "Rating", "Date")

train_all <- read.table("C:\\Users\\MrW\\Desktop\\Cassy's School Stuff\\MRP\\training_set.tar", header=TRUE,sep = ",",skip = 1, fill = TRUE)
colnames(train4)<- c("CustomerID", "Rating", "Date")

probe <- read.table("C:\\Users\\MrW\\Desktop\\Cassy's School Stuff\\MRP\\probe.txt",sep = ",")
qualifying <- read.table("C:\\Users\\MrW\\Desktop\\Cassy's School Stuff\\MRP\\qualifying.txt",sep = ",")



#######################################
##Function for Convert Trainign Data Set
#######################################
##Function to convert training data sets
##Currently sets do not include movie ID in the entry but rather list movie id as it's own entry and all ratings after
convertMovieID<-function(df, firstID){
  ##Make new frame to be returned
  newDF<-data.frame(matrix(ncol=4,nrow=0))
  colNames<-c("MovieID", "CustomerID", "Rating", "Date")
  colnames(newDF)<-colNames
  
  ##Since Read files got rid of first movie ID, it will be given to funciton
  currMovieID<-firstID
  for (entryIndex in c(1:nrow(df))){
    ##If the entry is the movie ID, update
    if(is.na(df[entryIndex,2])){
      currMovieID<-substr(toString(df[entryIndex,1]), 1, nchar(toString(df[entryIndex,1]))-1)
    }
    ##Else add entry to newDF with current movieID
    else{
      newDF[nrow(newDF)+1,]=list(currMovieID, df[entryIndex,1], df[entryIndex,2], toString(df[entryIndex,3]))
    }
  }
  return(newDF)
}

#######################################
##movie_titles Exploratory Analysis
#######################################
##Get move years from data frame
movie_years<-movie_titles$Year

##Find mean of movie release year
mean(movie_years)

##Plot histogram of movie release year
hist(movie_years, 
     main="Histogram for Movie Years", 
     xlab="Year", 
     border="black", 
     col="red",
     xlim=c(1890,2020),
     breaks=10)

##Plot boxplot of movie release year
boxplot(movie_years, main="Movie Years", ylab="Year")

#######################################
## Train Set Exploratory Analysis
#######################################
totalOriginal<-rbind(train1 ,train2,train3,train4)
subOriginal<-convertMovieID(totalOriginal,1)

##Get random sample from each text file by first getting a subset of customers
##5% -> 25,000 Customers
customerIDs<-sample.int(2649429, 40000)
cust_subset<-subOriginal[subOriginal$CustomerID %in% customerIDs,]

nrow(cust_subset)

##Check num of unique
customers<-cust_subset$CustomerID
n_Customer<-unique(customers)
numC<-length(n_Customer)
numC

##Get a sample of movies from these customers
movie<-cust_subset$MovieID
n_Movie<-unique(movie)
numN<-length(n_Movie)
numN



##Combine random sample to new Training set
totalTrain<-cust_subset
colNames<-c("MovieID", "CustomerID", "Rating", "Date")
colnames(totalTrain)<-colNames
 
totalTrain
##Plot histogram of movie rating from total training data
hist(totalOriginal$Rating, 
     col="Red",
     main="Histogram for Ratings in Total", 
     xlab="Rating")

##Plot boxplot of movie rating from total Training data
boxplot(totalOriginal$Rating, main="Movie Ratings from all Data", ylab="Rating")
mean(totalOriginal$Rating)
sd(totalOriginal$Rating)


totalTrain[,3]<-sapply(totalTrain[,3],as.numeric)
totalTrain[,3]
##Plot histogram of movie rating from subset  training data
hist(totalTrain$Rating, 
     col="Red",
     main="Ratings in Training", 
     xlab="Rating")

##Plot boxplot of movie rating from subset Training data
boxplot(totalTrain$Rating, main="Movie Ratings from Training", ylab="Rating")
mean(totalTrain$Rating)
sd(totalTrain$Rating)

##Find number of unique movies
movies<-totalTrain$MovieID
nUnique_Movie<-unique(movies)
n<-length(nUnique_Movie)
n

##Merge movie data set and training set to get movie release along with rating to do correlation
FullTrain<-merge(movie_titles, totalTrain, on='MovieID')


##Corrlation matrix of full training set
df2<-FullTrain
startdate <- as.Date("01/01/2000","%d/%m/%Y")
df2$Date <- difftime(df2$Date,startdate,unit="days")
df1<-df2[,c('Rating', 'Year','Date')]
df1[, c(3)] <- sapply(df1[, c(3)], as.numeric)
corrplot(cor(df1), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

##Find average ratings for each movie
df<- FullTrain[,c('MovieID','Rating')]
newdf <- df %>% group_by(MovieID) %>% summarise_each(funs(mean))
newdf
AverageRating<-merge(movie_titles, newdf, on='MovieID')
movie_rate<-AverageRating[,c('Rating', 'Movie')]

movie_rate<-movie_rate[order(movie_rate$Rating),]

movie_rate

FullTrain
FullTrain[,4]<-sapply(FullTrain[,4],as.numeric)

##Save dataset for future use
write.csv(FullTrain,"C:\\Users\\MrW\\Desktop\\Cassy's School Stuff\\MRP\\FullTrain.csv")
