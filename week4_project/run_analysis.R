################################################################################
# Loading Packages
################################################################################

library(dplyr)


################################################################################
# Downloading and Unpacking Data
################################################################################

zip_url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
zip_name <- 'UCI_HAR_Dataset.zip'

# if the file is not in the directory download it
if (!file.exists(zip_name)){
    download.file(zip_url, zip_name, mode = 'wb', method = 'curl')
}

data_path <- 'UCI_HAR_Dataset'

# if the directory don't exists unzip the file
if (!dir.exists(data_path)){
    unzip(zip_name)
}

################################################################################
# Reading Data
################################################################################

# Read training data
xtrain <- read.table(file.path(data_path, 'train', 'X_train.txt'))
ytrain <- read.table(file.path(data_path, 'train', 'Y_train.txt'))
subject_train <- read.table(file.path(data_path, 'train', 'subject_train.txt'))

# Read testing data
xtest <- read.table(file.path(data_path, 'test', 'X_test.txt'))
ytest <- read.table(file.path(data_path, 'test', 'Y_test.txt'))
subject_test <- read.table(file.path(data_path, 'test', 'subject_test.txt'))

# Read feature data
features <- read.table(file.path(data_path, 'features.txt'))

# Read activity labels data
activity_labels <- read.table(file.path(data_path, 'activity_labels.txt'))
colnames(activity_labels) <- c('activityId', 'activityType')

# Renaming the columns
colnames(xtrain) <- features[, 2]
colnames(ytrain) <- 'activityId'
colnames(subject_train) <- 'subjectId'

colnames(xtest) <- features[, 2]
colnames(ytest) <- 'activityId'
colnames(subject_test) <- 'subjectId'

################################################################################
# STEP 1:   Merging training and testing data into one dataset
################################################################################

# merging datasets
merged_df <- rbind(
    cbind(subject_train, xtrain, ytrain), 
    cbind(subject_test, xtest, ytest)
)

################################################################################
# STEP 2: Extracting only the measurements on the mean and standard deviation 
#         for each measurement
################################################################################

# get column names of the columns to keep
columnstokeep <- grepl('subjectId|activityId|mean|std', colnames(merged_df))

# extracting columns for dataset
merged_on_mean_and_std <- merged_df[, columnstokeep]

################################################################################
# STEP 3: Use descriptive activity names to name the activities in the data set
################################################################################

merged_on_activity <- merge(merged_on_mean_and_std, activity_labels, 
                            by = 'activityId')

################################################################################
# STEP 4: Appropriately label the data set with descriptive variable names
################################################################################

# get column names
mergedCols <- colnames(merged_on_mean_and_std)

# remove special characters
mergedCols <- gsub("[\\(\\)-]", "", mergedCols)

# expand abbreviations and clean up names
mergedCols <- gsub("^f", "frequencyDomain", mergedCols)
mergedCols <- gsub("^t", "timeDomain", mergedCols)
mergedCols <- gsub("Acc", "Accelerometer", mergedCols)
mergedCols <- gsub("Gyro", "Gyroscope", mergedCols)
mergedCols <- gsub("Mag", "Magnitude", mergedCols)
mergedCols <- gsub("Freq", "Frequency", mergedCols)
mergedCols <- gsub("mean", "Mean", mergedCols)
mergedCols <- gsub("std", "StandardDeviation", mergedCols)

# correct typo
mergedCols <- gsub("BodyBody", "Body", mergedCols)

# use new labels as column names
colnames(merged_on_mean_and_std) <- mergedCols

################################################################################
# STEP 5: Create an independent tidy set with the average of each variable for 
#         each subject and each activity
################################################################################

# group by subject and activity and take mean
merged_on_activity_mean <- merged_on_activity %>% 
    group_by(subjectId, activityId) %>% 
    summarise_each(funs(mean))

# output to file 'tidy_data.txt'
write.table(merged_on_activity_mean, 'tidy_data.txt', row.names = FALSE)


################################################################################