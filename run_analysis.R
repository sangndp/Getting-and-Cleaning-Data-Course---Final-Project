library(dplyr)
library(tidyr)
library(plyr)

### Download data ###
file_link <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file_path <- "F:/MOOC/Data Science Specialization/Getting and Cleaning Data"
setwd(file_path)
if(!file.exists("./data")){dir.create("./data")}
download.file(file_link, destfile = "./data/Dataset.zip")
folder <- unzip(zipfile = "./data/Dataset.zip", exdir = "./data") # unzip folder

# import data #
x_test <- tbl_df(read.table("./data/UCI HAR Dataset/test/X_test.txt")) # import x_test
y_test <- tbl_df(read.table("./data/UCI HAR Dataset/test/y_test.txt")) # import y_test
subject_test <- tbl_df(read.table("./data/UCI HAR Dataset/test/subject_test.txt")) # import subject_test

x_train <- tbl_df(read.table("./data/UCI HAR Dataset/train/X_train.txt")) # import x_train
y_train <- tbl_df(read.table("./data/UCI HAR Dataset/train/y_train.txt")) # import y_train
subject_train <- tbl_df(read.table("./data/UCI HAR Dataset/train/subject_train.txt")) # import subject_train

feature_names <- tbl_df(read.table("./data/UCI HAR Dataset/features.txt")) # import feature names
activity_labels <- tbl_df(read.table("./data/UCI HAR Dataset/activity_labels.txt")) # import activity labels

# Q1: Merges the training and the test sets to create one data set #
x_data <- rbind(x_test, x_train)
y_data <- rbind(y_test, y_train)
subject_data <- rbind(subject_test, subject_train)

names(subject_data) <- c("subject")
names(y_data) <- c("activity")
names(x_data) <- feature_names$V2

full_data <- cbind(x_data, y_data, subject_data)

# Q2: Extracts only the measurements on the mean and standard deviation for each measurement #
selected_data <- full_data[, grepl("(std|mean)\\(\\)", names(full_data))] # select mean and standard deviation for each measurment
data <- cbind(selected_data, subject_data, y_data) # data has only the measurements on the mean and standard deviation for each measurement

# Q3: Uses descriptive activity names to name the activities in the data set #
lookup <- data.frame(activity=c(1,2,3,4,5,6), Label=c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")) # setting lookup table
data <- join(data, lookup, by = "activity") # to lookup data to name the activities in the data set

# Q4: Appropriately labels the data set with descriptive variable names #
names(data) <- gsub("^t", "time", names(data))
names(data) <- gsub("^f", "frequency", names(data))
names(data) <- gsub("Mag", "Magnitude", names(data))
names(data) <- gsub("Acc", "Acceleration", names(data))
names(data) <- gsub("Gyro", "Gyroscope", names(data))

# Q5: creates a second, independent tidy data set with the average of each variable for each activity and each subject #
tidy_data <- aggregate(data, by = list(data$subject, data$activity), FUN = mean) # group data and average of each variable for each activity and each subject
tidy_data <- tidy_data[order(tidy_data$subject, tidy_data$activity), -c(1,2) ] # order data by subject andactivity
str(tidy_data) # check data set
write.table(tidy_data, file = "tidydata.txt",row.name=FALSE) # creates a second, independent tidy data set
