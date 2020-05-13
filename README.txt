# Getting-Cleaning-Data-Assignment
Project to collect, work with, and clean a data set

#You should create one R script called run_analysis.R that does the following.

#1# Merges the training and the test sets to create one data set.
#2# Extracts only the measurements on the mean and standard deviation for each measurement.
#3# Uses descriptive activity names to name the activities in the data set
#4# Appropriately labels the data set with descriptive variable names.
#5# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#  step 1: Identify files to be processed:

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

# step 2 Read tables using fread

# step 3 Rename column names

# step 4 Bind train and test dataframes in 2 data frames

# step 5 rbind train and test into 1 df

# step 6 add column with activity names

# step 7 filter columns  with sum and standard deviation

# step 8 create new df with grouped sums

# writre data frame into csv file
