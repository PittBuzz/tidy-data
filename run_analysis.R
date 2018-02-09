##1. Merges the training and the test sets to create one data set.
## loading data

setwd("C:/Users/Jack's PC/Desktop/data science/wk4")
library(plyr)
library(data.table)
subjectTrain = read.table('./subject_train.txt',header=FALSE)
xTrain = read.table('./x_train.txt',header=FALSE)
yTrain = read.table('./y_train.txt',header=FALSE)

subjectTest = read.table('./subject_test.txt',header=FALSE)
xTest = read.table('./x_test.txt',header=FALSE)
yTest = read.table('./y_test.txt',header=FALSE)

##merging data into one set                   

xDataSet <- rbind(xTrain, xTest)
yDataSet <- rbind(yTrain, yTest)
subjectDataSet <- rbind(subjectTrain, subjectTest)
dim(xDataSet)   
dim(yDataSet)
dim(subjectDataSet)

##2. Extract only the measurements on the mean and standard deviation for each measurement.

xDataSet_mean_std <- xDataSet[, grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2])]
names(xDataSet_mean_std) <- read.table("features.txt")[grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2]), 2] 
View(xDataSet_mean_std)
dim(xDataSet_mean_std)

##3. Use descriptive activity names to name the activities in the data set.

yDataSet[, 1] <- read.table("activity_labels.txt")[yDataSet[, 1], 2]
names(yDataSet) <- "Activity"
View(yDataSet)

##4. Appropriately label the data set with descriptive activity names.

names(subjectDataSet) <- "Subject"
summary(subjectDataSet)

#makes new single data set

NewData <- cbind(xDataSet_mean_std, yDataSet, subjectDataSet)

names(NewData) <- make.names(names(NewData))
names(NewData) <- gsub('Acc',"Acceleration",names(NewData))
names(NewData) <- gsub('Gyro',"Gyroscope",names(NewData))
names(NewData) <- gsub('Mag',"Magnitude",names(NewData))
names(NewData) <- gsub('^t',"TimeDomain",names(NewData))
names(NewData) <- gsub('^f',"FrequencyDomain",names(NewData))
names(NewData) <- gsub('\\.mean',".Mean",names(NewData))
names(NewData) <- gsub('\\.std',".StandardDeviation",names(NewData))
names(NewData) <- gsub('Freq\\.',"Frequency.",names(NewData))

View(NewData)
##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

names(NewData)

AvData<-aggregate(. ~Subject + Activity, NewData, mean)
AvData<-AvData[order(AvData$Subject,AvData$Activity),]
write.table(AvData, file = "tidydataset.txt",row.name=FALSE)