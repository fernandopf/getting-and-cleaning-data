
## First 
# download zip file containing data if it hasn't already been downloaded
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zip <- "UCI HAR Dataset.zip"

if (!file.exists(zip)) {
  download.file(url, zip, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"

if (!file.exists(dataPath)) {
  unzip(zip)
}

# Activities
Activities <- read.table(file.path(dataPath,"activity_labels.txt"))
names(Activities) <- c("NActivities", "Activities")

# Features
Features <- read.table(file.path(dataPath,"activity_labels.txt"), as.is = TRUE)


# Training
training <- read.table(file.path(dataPath, "train", "subject_train.txt"))
X_Train <- read.table(file.path(dataPath, "train" ,"X_train.txt"))
names(X_Train) <- "X_Train"
Y_Train <- read.table(file.path(dataPath, "train" ,"y_train.txt"))
names(Y_Train) <- "NActivities"

# Test
test <- read.table(file.path(dataPath, "test", "subject_test.txt"))
X_Test <- read.table(file.path(dataPath, "test" ,"X_test.txt"))
names(X_Test) <- "X_test"
Y_Test <- read.table(file.path(dataPath, "test" ,"y_test.txt"))
names(Y_Test) <- "NActivities"



# Merges the training and the test sets to create one data set.

Training_Data_Set <- cbind(training, Y_Train, X_Train)
Test_Data_Set <- cbind(test, Y_Test, X_Test)
names(Training_Data_Set)=names(Test_Data_Set) #ensure names are the same

Final_Data_Set <- rbind(Training_Data_Set, Test_Data_Set,make.row.names	
=TRUE)

# remove individual data tables to save memory
rm(X_Train, Y_Train, Training_Data_Set, X_Test, Y_Test, Test_Data_Set, test, training)

coln <- c("Subject","Activities",features[,2])
names(Final_Data_Set) <- coln

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
goodCol <- grepl("Mean|Std|mean|std", coln)
goodCol[1:2] = TRUE
goodData <- Final_Data_Set[,goodCol]

# 3. Uses descriptive activity names to name the activities in the data set

goodData[,2] <-factor(goodData[,2], levels = Activities[,1], labels= Activities[,2])

# 4. Appropriately labels the data set with descriptive variable names.
colums <- names (goodData)
colums <- gsub("^t", "Time", colums)
column <- gsub("^f", "Frequency", colums)
column <- gsub("Acc", "Accelerometer", colums)
column <- gsub("Gyro", "Gyroscope", colums)
column <- gsub("Mag", "Maginitude", colums)
column <- gsub("mean", "Mean", colums)
column <- gsub("Freq", "Frequency", colums)
column <- gsub("std", "StandartDesviation", colums)
names(goodData) <- column

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(reshape2)

# create the tidy data set
m <- melt(goodData, id=c("Subject","Activities"))
F_Data_Set <- as.data.frame(dcast(m, Subject+Subject ~ variable, mean))

