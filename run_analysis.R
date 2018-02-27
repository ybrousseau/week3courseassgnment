setwd("C:/Users/ybrousseau/Desktop/Trainings_Learning/JHU_Coursera/course3/week4data/UCI HAR Dataset")

install.packages("dplyr", dependencies = TRUE)
install.packages("reshape2", dependencies = TRUE)
library(dplyr)
library(reshape2)

# Merges the training and the test sets to create one data set.
# X data
x.train <- read.table("C:/Users/ybrousseau/Desktop/Trainings_Learning/JHU_Coursera/course3/week4data/UCI HAR Dataset/train/X_train.txt")
x.test <- read.table("C:/Users/ybrousseau/Desktop/Trainings_Learning/JHU_Coursera/course3/week4data/UCI HAR Dataset/test/X_test.txt")
features <- read.table("C:/Users/ybrousseau/Desktop/Trainings_Learning/JHU_Coursera/course3/week4data/UCI HAR Dataset/features.txt")
colnames(x.train) <- features[,2]
colnames(x.test) <- features[,2]

# Subject data
subject.test <- read.table("C:/Users/ybrousseau/Desktop/Trainings_Learning/JHU_Coursera/course3/week4data/UCI HAR Dataset/test/subject_test.txt")
sub

ject.train <- read.table("C:/Users/ybrousseau/Desktop/Trainings_Learning/JHU_Coursera/course3/week4data/UCI HAR Dataset/train/subject_train.txt")
names(subject.test) <- "subject.ID"
names(subject.train) <- "subject.ID"

# Y data
y.train <- read.table("C:/Users/ybrousseau/Desktop/Trainings_Learning/JHU_Coursera/course3/week4data/UCI HAR Dataset/train/Y_train.txt")
y.test <- read.table("C:/Users/ybrousseau/Desktop/Trainings_Learning/JHU_Coursera/course3/week4data/UCI HAR Dataset/test/Y_test.txt")
colnames(y.train) <- "activity.ID"
colnames(y.test) <- "activity.ID"

# Activity Data 
activity.labels <- read.table("C:/Users/ybrousseau/Desktop/Trainings_Learning/JHU_Coursera/course3/week4data/UCI HAR Dataset/activity_labels.txt")
colnames(activity.labels) <- c("activity.ID", "activity.Type")

# Merge data 
train.data <- cbind(subject.train,y.train,x.train)
test.data <- cbind(subject.test,y.test,x.test)
combined.data <- rbind(train.data, test.data)

# Extracts only the measurements on the mean and standard deviation for each measurement
select <- grepl("-mean()|-std()", colnames(combined.data))
select <- c(TRUE,TRUE,select)
combined.data.trimmed <- combined.data[, select]


# (3) Uses descriptive activity names to name the activities in the data set
combined.data.trimmed <- merge(combined.data.trimmed, activity.labels, by = "activity.ID")


# (4) Appropriately labels the data set with descriptive variable names
columns <- names(combined.data.trimmed)

# remove parentheses and hyphens
columns <- gsub("\\()","", columns)

# add descriptive column names 
columns <- gsub("^f", "frequencydomain", columns)
columns <- gsub("^t", "timedomain", columns)
columns <- gsub("mean", "mean", columns)
columns <- gsub("std", "standarddeviation", columns)
columns <- gsub("Acc", "accelerometer", columns)
columns <- gsub("Mag", "Magnitude", columns)
columns <- gsub("Freq", "frequency", columns)
columns <- gsub("Gyro", "gyroscope", columns)

# assign new column names 
names(combined.data.trimmed) <- columns


# (5) From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each activity and each subject
tidy.means <- combined.data.trimmed %>% 
  group_by(subject.ID, activity.ID) %>%
  summarise_each(funs(mean))

summary(tidy.means)
dim(tidy.means)
str(tidy.means)

write.table(tidy.means, "tidy_means.txt", row.names = FALSE)
