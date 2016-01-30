#######################################################################

# Coursera Getting and Cleaning Data Course Project

# January 2015
# File Description:

# This script will do the following: 

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation 
#    for each measurement.
# 3. Uses descriptive activity names to name the activities in the data 
#    set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy 
#    data set with the average of each variable for each activity and
#    each subject.

# to the file downloaded from

# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# as part of the course "Getting and Cleaning Data"

# Please excuse my poor english, but it is not my native language

#######################################################################

# Startup clean and set working directory
rm(list=ls())
setwd('/home/us/Documents/mooc/cleaning_data/Tarea_Final/UCI_HAR_Dataset');

# 1. Merges the training and the test sets to create one data set.

# Load the data
features     	= read.table('./features.txt',header=FALSE); 
activityType = read.table('./activity_labels.txt',header=FALSE);
subjectTrain 	= read.table('./train/subject_train.txt',header=FALSE);
train_x       = read.table('./train/X_train.txt',header=FALSE);
train_y       = read.table('./train/y_train.txt',header=FALSE);

# Assign column names
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(train_x) = features[,2]; 
colnames(train_y) = "activityId";

# Create the training full set by merging train_y, subject_train, and train_x
trainingData = cbind(train_y,subjectTrain,train_x);

# Read in the test data
subjectTest  = read.table('./test/subject_test.txt',header=FALSE);
test_x       = read.table('./test/X_test.txt',header=FALSE);
test_y       = read.table('./test/y_test.txt',header=FALSE);

# Assign column names
colnames(subjectTest)  = "subjectId";
colnames(test_x)       = features[,2]; 
colnames(test_y)       = "activityId";


# Merge the test_x, test_y and subjectTest data
testData = cbind(test_y,subjectTest,test_x);

# Merge training and test data 
finalData = rbind(trainingData,testData);

# Create a vector for the column names, which will be used to select the
# desired mean() & stddev()
colnames  = colnames(finalData); 

# Create a vector to select columns ID, mean() & stddev()
select_vector = (grepl("activity..",colnames) | grepl("subject..",colnames) | grepl("-mean..",colnames) & !grepl("-meanFreq..",colnames) & !grepl("mean..-",colnames) | grepl("-std..",colnames) & !grepl("-std()..-",colnames));

# Save the selected columns of finalData
finalData = finalData[select_vector==TRUE];

# 3. Uses descriptive activity names to name the activities in the data 
#    set

# Merge the finalData set with the activityType table to include
# descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# Updating the colnames vector to include the new column names after merge
colnames  = colnames(finalData); 

# 4. Appropriately labels the data set with descriptive variable names.

# Cleaning the variable names
for (i in 1:length(colnames)) 
{
        colnames[i] = gsub("\\()","",colnames[i])
        colnames[i] = gsub("-std$","StdDev",colnames[i])
        colnames[i] = gsub("-mean","Mean",colnames[i])
        colnames[i] = gsub("^(t)","time",colnames[i])
        colnames[i] = gsub("^(f)","freq",colnames[i])
        colnames[i] = gsub("([Gg]ravity)","Gravity",colnames[i])
        colnames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colnames[i])
        colnames[i] = gsub("[Gg]yro","Gyro",colnames[i])
        colnames[i] = gsub("AccMag","AccMagnitude",colnames[i])
        colnames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colnames[i])
        colnames[i] = gsub("JerkMag","JerkMagnitude",colnames[i])
        colnames[i] = gsub("GyroMag","GyroMagnitude",colnames[i])
};

# Reassigning the new of columns names to the finalData set
colnames(finalData) = colnames;

# 5. From the data set in step 4, creates a second, independent tidy 
#    data set with the average of each variable for each activity and
#    each subject.

# Create a new table, finalDataNoactivityType without the
# activityType column
finalDataNoactivityType  = finalData[,names(finalData) != 'activityType'];

# Summarizing the finalDataNoactivityType table to include just the
# mean of each variable for each activity and each subject
tidyData = aggregate(finalDataNoactivityType[,names(finalDataNoactivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoactivityType$activityId,subjectId = finalDataNoactivityType$subjectId),mean);

# Merge tidyData with activityType to include descriptive activity
# names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the last data set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');

