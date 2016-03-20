#This script uses the UCI HAR Dataset 
# (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) to:
# a) Merge the training and the test sets to create one data set.
# b) Extract only the measurements on the mean and standard deviation for each measurement. 
# c) Use descriptive activity names to name the activities in the data set
# d) Appropriately labels the data set with descriptive variable names.
# e) Create a second, independent tidy data set with the average of each variable for each activity and each subject.

####### a) Merge the training and the test sets to create one data set
# Set Working directory
setwd("/Users/hhvandi/Desktop/Coursera Data Science Specialization/Getting and Cleaning Data/Course Project/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/")

# Import test data
features <- read.table("./features.txt", header = FALSE)
subject_test <- read.table("./test/subject_test.txt",header=FALSE)
x_test <- read.table("./test/x_test.txt",header=FALSE)
y_test <- read.table("./test/y_test.txt",header=FALSE)

# Name test data columns
colnames(subject_test) = "Subject_ID"
colnames(x_test) = features[,2]
colnames(y_test) = "Activity_ID"

# Merge subject_test, x_test, and y_test
Testing <- cbind(subject_test, x_test, y_test)

# Read train data into R
activity_type <- read.table("./activity_labels.txt", header = FALSE)
features <- read.table("./features.txt", header = FALSE)
subject_train <- read.table("./train/subject_train.txt", header = FALSE)
x_train <- read.table("./train/x_train.txt", header = FALSE)
y_train <- read.table("./train/y_train.txt", header = FALSE)

#Name train data columns
colnames(activity_type) = c("Activity_ID", "Activity_Type")
colnames(subject_train) = "Subject_ID"
colnames(x_train) = features[,2]
colnames(y_train) = "Activity_ID"

# Merge subject_train, x_train, and y_train
Training<- cbind(subject_train, x_train, y_train)

# Create final dataset by combining Testing and Training
my_data<- rbind(Testing, Training)

# Vector for the column names from combined_data
labels<- colnames(my_data)

####### b) Extract only the mean and standard deviation
# Generate a logical vector that is TRUE for the ID, mean and standard deviation columns and FALSE for others
logical_vector<- (grepl("Activity..", labels) | grepl("Subject", labels) | grepl("-mean..", labels) & !grepl("-meanFreq..", labels))

# Subset desired columns
my_data<- my_data[logical_vector == TRUE]


######## c) Name the activities in the data set using descriptive activity names
#Merge my_data with activity_type
final <- merge(my_data, activity_type, by = "Activity_ID", all.x=TRUE)
column_names<- colnames(my_data)

####### d) Label the data set with descriptive activity names
# Create a list of the current column names and feature names and then tidy the list
names(final)
names(final) <- gsub("Acc", "Accelerator", names(final))
names(final) <- gsub("Mag", "Magnitude", names(final))
names(final) <- gsub("Gyro", "Gyroscope", names(final))
names(final) <- gsub("^t", "time", names(final))
names(final) <- gsub("^f", "frequency", names(final))


####### e) Create a second, independent tidy data set with the average of each variable for each activity and each subject.
library(data.table)
final$Subject_ID <-as.factor(final$Subject_ID)
final<- data.table(final)
# Create tidy data set
Tidy_Data <- aggregate(.~Subject_ID + Activity_ID, final, mean)
Tidy_Data <- Tidy_Data[order(Tidy_Data$Subject_ID,Tidy_Data$Activity_ID),]
write.table(Tidy_Data, file = "Tidy.txt", row.names = FALSE)