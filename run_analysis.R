setwd("/Users/mckenzieyoung/Desktop/Coursera/r2015")
library(foreign)
library(xlsx)
library(plyr)
library(data.table)
dataset_url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(dataset_url, "assign2.zip")
unzip("assign2.zip", exdir = "assign2")

####PROBLEM 1###Merge the training and the test sets to create one data set.######

######Load features and activity type#######
mydata.feat <- read.table("UCI HAR Dataset/features.txt",header=FALSE)
mydata.act <- read.table("UCI HAR Dataset/activity_labels.txt",header=FALSE)
names(mydata.act) <- c("Activity","Activity.Type")

#######Load Data#######
######Training#########
mydata.trx <- read.table("UCI HAR Dataset/train/X_train.txt",header=FALSE)
##############Set variable names from features txt file########
names(mydata.trx) <- mydata.feat$V2
###2####Extract only the measurements on the mean and standard deviation for each measurement
######Use grep###### "\\" tells r symbols are not metacharacters but should be treated as regular expressions#######
mslogic <- c("\\-mean\\(\\)","\\-std\\(\\)")
mydata.2 <- grep(paste(mslogic,collapse="|"),mydata.feat$V2)
##########
####Subset mean/std columns from mydata.2 vector#########
mydata.trx <- mydata.trx[,mydata.2]
#nrow(mydata.try)
######################
mydata.try <- read.table("UCI HAR Dataset/train/y_train.txt",header=FALSE)
names(mydata.try) <- "Subject.ID"
mydata.trh <- read.table("UCI HAR Dataset/train/subject_train.txt",header=FALSE)
names(mydata.trh) <- "Activity"
training <- cbind(mydata.trx, mydata.try, mydata.trh)
head(training)

#########Test########
mydata.tex <- read.table("UCI HAR Dataset/test/X_test.txt",header=FALSE)
#set variable names from features txt file
names(mydata.tex) <- mydata.feat$V2
###########Extract mean and std like in training set######
mydata.tex <- mydata.tex[,mydata.2]
mydata.tey <- read.table("UCI HAR Dataset/test/y_test.txt",header=FALSE)
names(mydata.tey) <- "Subject.ID"
mydata.teh <- read.table("UCI HAR Dataset/test/subject_test.txt",header=FALSE)
names(mydata.teh) <- "Activity"
test <- cbind(mydata.tex, mydata.tey, mydata.teh)
#names(test)
combo_tt <- rbind(training,test) 
dim(combo_tt)
###########
#head(mydata.try)
#nrow(mydata.teh)
#ncol(mydata.teh)


###3####Use descriptive activity names to name the activities in the data set
mydata.3 <- merge(combo_tt,mydata.act,by='Activity',all.x=TRUE)
names(mydata.3)

###4###Appropriately labels the data set with descriptive variable names. 
colnam <- colnames(mydata.3)
########gsub is version of grep that replaces elements#######
colnam <- gsub("\\-mean\\(\\)","Mean",colnam)
colnam <- gsub("\\-std\\(\\)","StdDev",colnam)
########^ tells gsub to only replace beginning of string ($ is for end of a line)#####
colnam <- gsub("^t","Time",colnam)
colnam <- gsub("^f","Freq",colnam)
colnames(mydata.3) <- colnam
colnames(mydata.3)

####5###From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
######Remove Activity Names########
mydata.5 <- mydata.3[,-(ncol(mydata.3))]
#names(mydata.5)
ncol(mydata.5)
#######Organize data by all combinations of activity and subject (180) and calculate the mean for each numeric variable#####
tidy.data <- aggregate(mydata.5[,2:67], by=list(mydata.5$Activity,mydata.5$Subject.ID), data=mydata.5, FUN=mean)
#########
write.table(tidy.data, "TidyData.txt", row.names=FALSE, quote=FALSE)