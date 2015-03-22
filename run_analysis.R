# 1. Merge the training and the test sets to create one data set.

#set working directory to the location where the UCI HAR Dataset was unzipped
setwd('C:/Users/Computer/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset')


# Read in the training data
features = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt

# Assign names to columns
colnames(activityType) = c('activityId','activityType');
colnames(subjectTrain) = "subjectId";
colnames(xTrain) = features[,2]; 
colnames(yTrain) = "activityId";

# Create full training data
FullTrain = cbind(yTrain,subjectTrain,xTrain);

# Read in the test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Assign column names to the test data imported above
colnames(subjectTest) = "subjectId";
colnames(xTest) = features[,2]; 
colnames(yTest) = "activityId";

# Create full test data
FullTest = cbind(yTest,subjectTest,xTest);

# Combine full training and full test data
FullSet = rbind(FullTrain,FullTest);

# Create a vector for the column names from the FullSet, which will be used
# to select the desired mean() & stddev() columns
Header = colnames(FullSet); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create new object with TRUE values for the ID, mean() & stddev() columns and FALSE for others
NewObject = (grepl("activity..",Header) | grepl("subject..",Header) | grepl("-mean..",Header) & !grepl("-meanFreq..",Header) & !grepl("mean..-",Header) | grepl("-std..",Header) & !grepl("-std()..-",Header));

# Subset FullSet table based on the NewObject to keep only desired columns
FullSet = FullSet[NewObject==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the FullSet set with the acitivityType table to include descriptive activity names
FullSet = merge(FullSet,activityType,by='activityId',all.x=TRUE);

# Update Header so new columns show up
Header = colnames(FullSet); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(Header)) 
{
  Header[i] = gsub("\\()","",Header[i])
  Header[i] = gsub("-std$","StdDev",Header[i])
  Header[i] = gsub("-mean","Mean",Header[i])
  Header[i] = gsub("^(t)","time",Header[i])
  Header[i] = gsub("^(f)","freq",Header[i])
  Header[i] = gsub("([Gg]ravity)","Gravity",Header[i])
  Header[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",Header[i])
  Header[i] = gsub("[Gg]yro","Gyro",Header[i])
  Header[i] = gsub("AccMag","AccMagnitude",Header[i])
  Header[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",Header[i])
  Header[i] = gsub("JerkMag","JerkMagnitude",Header[i])
  Header[i] = gsub("GyroMag","GyroMagnitude",Header[i])
};

# Reassign the new columns names to FullSet 
Header=colnames(FullSet);

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, FullSetNoActivityType without the activityType column
FullSetNoActType = FullSet[,names(FullSet) != 'activityType'];

# Summarizing the FullSetNoActivityType table to include just the mean of each variable for each activity and each subject
FinalSet=aggregate(FullSetNoActType[,names(FullSetNoActType) != c('activityId','subjectId')],by=list(activityId=FullSetNoActType$activityId,subjectId = FullSetNoActType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
FinalSet=merge(FinalSet,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(FinalSet, './FinalSet.txt',row.names=FALSE,sep='\t');
