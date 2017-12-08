####################################################################################################
# Pre Task: Load packages, download data, and unzip

## Load Packages and get the Data
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
## Download zip file from website
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "dataFiles.zip"))
## unzip data
unzip(zipfile = "dataFiles.zip")

####################################################################################################
# Main Task 1. Merges the training and the test sets to create one data set

## Read in the data from files
features     <- read.table('./UCI HAR Dataset/features.txt',header=FALSE); #imports features.txt
activityType <- read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain <- read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       <- read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       <- read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE); #imports y_train.txt

## Assigin column names to the data imported above
colnames(activityType)  <- c('activityId','activityType');
colnames(subjectTrain)  <- "subjectId";
colnames(xTrain)        <- features[,2]; 
colnames(yTrain)        <- "activityId";

## Create the final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);

## Read in the test data
subjectTest <- read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       <- read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       <- read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE); #imports y_test.txt

## Assign column names to the test data imported above
colnames(subjectTest) <- "subjectId";
colnames(xTest)       <- features[,2]; 
colnames(yTest)       <- "activityId";

## Create the final test set by merging the xTest, yTest and subjectTest data
testData <- cbind(yTest,subjectTest,xTest);

## Combine training and test data to create a final data set
fullData <- rbind(trainingData,testData);

####################################################################################################
# Main Task 2. Extracts only the measurements on the mean and standard deviation for each measurement

## step 1: load feature name into R
featureName <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)[,2]

## step 2:  extract mean and standard deviation of each measurements
featureIndex <- grep(("mean\\(\\)|std\\(\\)"), featureName)
finalData <- fullData[, c(1, 2, featureIndex+2)]
colnames(finalData) <- c("subject", "activity", featureName[featureIndex])

####################################################################################################
# Main Task 3. Uses descriptive activity names to name the activities in the data set

## step 1: load activity data into R
activityName <- read.table("./UCI HAR Dataset/activity_labels.txt")

## step 2: replace 1 to 6 with activity names
finalData$activity <- factor(finalData$activity, levels = activityName[,1], labels = activityName[,2])

####################################################################################################
# Main Task 4. Appropriately labels the data set with descriptive variable names
names(finalData) <- gsub("\\()", "", names(finalData))
names(finalData) <- gsub("^t", "time", names(finalData))
names(finalData) <- gsub("^f", "frequence", names(finalData))
names(finalData) <- gsub("-mean", "Mean", names(finalData))
names(finalData) <- gsub("-std", "Std", names(finalData))

####################################################################################################
# Main Task 5. From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.
library(dplyr)
groupData <- finalData %>%
        group_by(subject, activity) %>%
        summarise_each(funs(mean))
write.table(groupData, "./UCI HAR Dataset/tidyData.txt", row.names = FALSE)


