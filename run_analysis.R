##Function for train or test table creating
createTable<-function(X_filePath,Y_filePath,subject_filePath,activityLabels,featureLabels)
{
  #Reading X data
  X_table<-read.csv(X_filePath, sep = "", header = FALSE)
  #Renaming columns in accordance with feature labels
  names(X_table)<-featureLabels$V2
  #Selecting features names with the 'mean' or 'std' word in the name
  includedCols<-sapply(featureLabels$V2,FUN=function(x) { length(grep("mean",x))>0 | length(grep("std",x))>0} )
  #Remove columns except "mean" and "std" measurements
  X_table<-X_table[,includedCols]
  
  #Reading Y data (activity)
  Y_table<-read.csv(Y_filePath, sep = " ", header = FALSE)
  #Renaming column
  names(Y_table)<-"activity"
  #Setting activities names 
  Y_table$activity<-activityLabels[Y_table$activity,]$V2
  
  #Reading subject data (subject id)
  subject<-read.csv(subject_filePath, sep = " ", header = FALSE)
  #Renaming column in accordance with activity labels
  names(subject)<-"subject"
  
  #Merging data from all of the tables
  resultTable<-cbind(subject,X_table,Y_table)
  #Returning merged table
  resultTable
}

createResultTable<-function()
{
  #Reading activity labels
  activityLabels<-read.csv("activity_labels.txt", sep = " ", header = FALSE,stringsAsFactors=FALSE)
  #Reading features
  featureLabels<-read.csv("features.txt", sep = " ", header = FALSE, stringsAsFactors=FALSE)
  
  #Getting train data
  trainTable<-createTable("train\\X_train.txt","train\\y_train.txt","train\\subject_train.txt",
                          activityLabels,featureLabels)
  #Getting test data
  testTable<-createTable("test\\X_test.txt","test\\y_test.txt","test\\subject_test.txt",
                         activityLabels,featureLabels)
  #Merging train and test data
  rbind(trainTable,testTable)
}

#Getting result data
resultData<-createResultTable()
#Getting new data with the average of each variable for each activity and each subject
resultGroupedData<-aggregate(resultData[,2:80],by=list(resultData$subject,resultData$activity),
                             FUN=mean)

write.table(resultGroupedData, file="Tidy.txt", row.name=FALSE)