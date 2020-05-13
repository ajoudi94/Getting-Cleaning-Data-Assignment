# Data Collection Assignment Week 4

rm(list = ls())

library(tidyr)
library(dplyr)
library(data.table)



zip_url <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
raw_csv_name <- "rawdata.zip"
raw_link <- paste0(getwd(),"/",raw_csv_name)
if(!file.exists("rawdata.zip")){
        
        download.file(zip_url,raw_link)
        
}
if(!file.exists("UCI HAR Dataset")){
        unzip(raw_link)
        
        
}

#You should create one R script called run_analysis.R that does the following.

#1# Merges the training and the test sets to create one data set.
#2# Extracts only the measurements on the mean and standard deviation for each measurement.
#3# Uses descriptive activity names to name the activities in the data set
#4# Appropriately labels the data set with descriptive variable names.
#5# From the data set in step 4, creates a second, independent tidy data set with the average of 
#each variable for each activity and each subject.

# Create usefull functions to avoid repetative tasks
folder_paths <- function(folder_link){
        #folder_link<-getwd()
        dir_files <- list.files(folder_link, recursive = T)
        dir_files <- paste0(folder_link,"/",dir_files)
        return(dir_files)
        
}# navigates through folders by getting folder's content's links
list_filter<- function(list,keyword){
#        getwd()
#        list<- dir_files
#        keyword <-"UCI HAR Dataset"
        raw_folder<- grep( keyword ,as.list(list),value = T)
        return(raw_folder)
} # filters through a vector/list for a specific keyword
read_data_link <- function(file_link, header = FALSE , sep =" "){
        data <-  read.delim(file_link,header=header,sep = sep)
        return(data)
        
}# reads text files

dir_files <- folder_paths(getwd())

#raw_folder_path <- list_filter(  dir_files,"UCI HAR Dataset")

#raw_folder<- folder_paths(raw_folder_path)

# Get activity lables to match later on
activity_lables_tab <-  read_data_link(list_filter(dir_files,"activity_labels.txt"),header=FALSE)
names(activity_lables_tab)<-  c("activity_id", "description")

# find data files to be extracted 

# Train sheets
xtrain_df <-fread( list_filter(dir_files,"/X_train.txt") ,header = F)        
ytrain_df <-fread( list_filter(dir_files,"/y_train.txt") ,header = F)        
subject_train <-fread( list_filter(dir_files,"subject_train.txt") ,header = F)
features <-read.table( list_filter(dir_files,"/features.txt") ,header = F)
# Test Sheets
xtest_df <-fread( list_filter(dir_files,"/X_test.txt") ,header = F)
ytest_df <-fread( list_filter(dir_files,"/y_test.txt") ,header = F)
subject_test <-fread( list_filter(dir_files,"subject_test.txt") ,header = F)      


#4# Appropriately labels the data set with descriptive variable names.

names(xtrain_df)<- sapply((features[,2]), as.character)
names(ytrain_df) <- "activity_id"
names(subject_train) <- "track_id"

names(xtest_df)<- sapply((features[,2]), as.character)
names(ytest_df) <- "activity_id"
names(subject_test) <- "track_id"

#1# Merges the training and the test sets to create one data set.
df_train <- cbind(xtrain_df,ytrain_df,subject_train)
df_test <- cbind(xtest_df,ytest_df,subject_test)
df_tidy_data<- as.data.frame (rbind(df_test,df_train))
#3# Uses descriptive activity names to name the activities in the data set

df_tidy_data$activity<- activity_lables_tab[match(df_tidy_data$activity_id,activity_lables_tab$activity_id),"description"]
?grepl
#2# Extracts only the measurements on the mean and standard deviation for each measurement.
column_names <- names(df_tidy_data)
filter_condition <-(grepl("activity",names(df_tidy_data))|grepl("track_id",names(df_tidy_data)) |
                            grepl("mean..",names(df_tidy_data)) |grepl("std..",names(df_tidy_data)))
df_std_mean<- df_tidy_data[,filter_condition == T]

#5# From the data set in step 4, creates a second, independent tidy data set with the average of 
#each variable for each activity and each subject.

df_tidyset <-tbl_df (aggregate(. ~track_id + activity, df_std_mean, mean) )

df_tidyset <- df_tidyset %>% arrange(track_id, activity_id)

write.csv(df_tidyset,"df_tidy_set.csv")
