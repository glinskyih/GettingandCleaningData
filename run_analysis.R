#Course Project

#Set working directory
setwd("P:/Data Science/Coursera/Getting and Cleaning Data")
getwd()

#1. Merge the training and the test sets to create one data set.

#Import data sets
train <- read.table("./UCI HAR Dataset/train/X_train.txt")
train.activity <- read.table("./UCI HAR Dataset/train/y_train.txt")
train.id <- read.table("./UCI HAR Dataset/train/subject_train.txt")
test <- read.table("./UCI HAR Dataset/test/X_test.txt")
test.activity <- read.table("./UCI HAR Dataset/test/y_test.txt")
test.id <- read.table("./UCI HAR Dataset/test/subject_test.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")

#Rename the ID columns in dataframe
library(plyr)
train.id <- rename(train.id,c("V1"="subject.id"))
train.activity <- rename(train.activity,c("V1"="activity"))
test.id <- rename(test.id,c("V1"="subject.id"))
test.activity <- rename(test.activity,c("V1"="activity"))

#Bind ID columns to data
train.final <- cbind(train.id,train.activity,train)
test.final <- cbind(test.id,test.activity,test)

#Merge train and test data sets
data <- rbind(train.final,test.final)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 

mean.cols <- grep("mean", features$V2) #Names of columns containing means
sd.cols <- grep("std", features$V2) #Names of columns containing standard deviations
keep.cols <- paste("V", sort(append(mean.cols, sd.cols)), sep="") #All mean and sd column names
data <- data[,c("subject.id","activity",keep.cols)] #Subsetting data to contain only mean and sd columns

#3. Uses descriptive activity names to name the activities in the data set

activity <- mapvalues(data$activity, from=1:6, to=as.character(activity$V2)) 
data$activity <- activity #Replace numeric activity with character string activity

#4. Appropriately labels the data set with descriptive variable names. 

new.names <- append(c("subject.id","activity"), as.character(features[sort(append(mean.cols, sd.cols)),2]), after=3)
names(data) <- new.names

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
#variable for each activity and each subject.

grp.means <- aggregate(.~subject.id+activity, data, function(x) mean(x))
grp.means <- grp.means[order(grp.means$subject.id,grp.means$activity),]
rownames(grp.means) <- NULL #Remove row names that saved from the ordering
write.table(grp.means, file="./Course Project/tidydataset.txt", row.names=F) #Write final tidy data set to text file
check <- read.table("./Course Project/tidydataset.txt", header = TRUE) #Test to see if the data table saved correctly
