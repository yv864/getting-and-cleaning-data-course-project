library(plyr)
library(dplyr)

setwd("~/UCI HAR Dataset")

# Load data
uci_hard_dir <- getwd()
feature_file <- paste(uci_hard_dir, "/features.txt", sep = "")
activity_labels_file <- paste(uci_hard_dir, "/activity_labels.txt", sep = "")
x_train_file <- paste(uci_hard_dir, "/train/X_train.txt", sep = "")
y_train_file <- paste(uci_hard_dir, "/train/y_train.txt", sep = "")
subject_train_file <- paste(uci_hard_dir, "/train/subject_train.txt", sep = "")
x_test_file  <- paste(uci_hard_dir, "/test/X_test.txt", sep = "")
y_test_file  <- paste(uci_hard_dir, "/test/y_test.txt", sep = "")
subject_test_file <- paste(uci_hard_dir, "/test/subject_test.txt", sep = "")

features <- read.table(feature_file, colClasses = c("character"))
activity_labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_file)
subject_train <- read.table(subject_train_file)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)
subject_test <- read.table(subject_test_file)

##################################################################
# 1. Merges the training and the test sets to create one data set.
##################################################################

training_dt <- cbind(cbind(x_train, subject_train), y_train)
test_dt <- cbind(cbind(x_test, subject_test), y_test)
sensor_dt <- rbind(training_dt, test_dt)

sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_dt) <- sensor_labels

############################################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
############################################################################################

sensor_dt_mean_std <- sensor_dt[,grepl("mean|std|Subject|ActivityId", names(sensor_dt))]

###########################################################################
# 3. Uses descriptive activity names to name the activities in the data set
###########################################################################

sensor_dt_mean_std1 <- join(sensor_dt_mean_std, activity_labels, by = "ActivityId", match = "first")
sensor_dt_mean_std2 <- sensor_dt_mean_std1[,-1]

##############################################################
# 4. Appropriately labels the data set with descriptive names.
##############################################################

names(sensor_dt_mean_std2) <- gsub('\\(|\\)',"",names(sensor_dt_mean_std2), perl = TRUE)
names(sensor_dt_mean_std2) <- make.names(names(sensor_dt_mean_std2))
names(sensor_dt_mean_std2) <- gsub('Acc',"Acceleration",names(sensor_dt_mean_std2))
names(sensor_dt_mean_std2) <- gsub('GyroJerk',"AngularAcceleration",names(sensor_dt_mean_std2))
names(sensor_dt_mean_std2) <- gsub('Gyro',"AngularSpeed",names(sensor_dt_mean_std2))
names(sensor_dt_mean_std2) <- gsub('Mag',"Magnitude",names(sensor_dt_mean_std2))
names(sensor_dt_mean_std2) <- gsub('^t',"TimeDomain.",names(sensor_dt_mean_std2))
names(sensor_dt_mean_std2) <- gsub('^f',"FrequencyDomain.",names(sensor_dt_mean_std2))
names(sensor_dt_mean_std2) <- gsub('\\.mean',".Mean",names(sensor_dt_mean_std2))
names(sensor_dt_mean_std2) <- gsub('\\.std',".StandardDeviation",names(sensor_dt_mean_std2))
names(sensor_dt_mean_std2) <- gsub('Freq\\.',"Frequency.",names(sensor_dt_mean_std2))
names(sensor_dt_mean_std2) <- gsub('Freq$',"Frequency",names(sensor_dt_mean_std2))

######################################################################################################################
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
######################################################################################################################

sensor_mean = ddply(sensor_dt_mean_std2, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_mean, file = "sensor_mean_results.txt")