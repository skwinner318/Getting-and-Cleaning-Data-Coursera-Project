library(dplyr)


##setting up data
filename <- "Getting_and_Cleaning_Project_Data"

if(!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename)
}

if(!file.exists("UCI HAR Dataset")){
  unzip(filename)
}

features <- read.table("UCI HAR Dataset/features.txt", col.names=c("n", "functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names= "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names=features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names="code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names="subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names=features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names="code")


##merging test and train data
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
full_data <- cbind(subject, X, Y)

##extracted means and st. devs
tidy <- full_data %>% select(subject, code, contains("mean"), contains("std"))

##name activities
tidy$code <- activities[tidy$code, 2]

##add labels
names(tidy)[2] = "activity"
names(tidy) <- gsub("Acc", "Accelerometer", names(tidy))
names(tidy) <- gsub("Gyro", "Gyroscope", names(tidy))
names(tidy) <- gsub("BodyBody", "Body", names(tidy))
names(tidy) <- gsub("Mag", "Magnitude", names(tidy))
names(tidy) <- gsub("^t", "Time", names(tidy))
names(tidy) <- gsub("^f", "Frequency", names(tidy))
names(tidy) <- gsub("tBody", "TimeBody", names(tidy))
names(tidy) <- gsub("-mean()", "Mean", names(tidy), ignore.case=TRUE)
names(tidy) <- gsub("-std()", "Standard Deviation", names(tidy), ignore.case=TRUE)
names(tidy) <- gsub("-freq()", "Frequency", names(tidy), ignore.case=TRUE)
names(tidy) <- gsub("angle", "Angle", names(tidy))
names(tidy) <- gsub("gravity", "Gravity", names(tidy))

##second data with averages
tidy2 <- tidy %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(tidy2, "Averages_Data.txt", row.name=FALSE)