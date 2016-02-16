if (!require("data.table")) {
    install.packages("data.table")
}

if (!require("reshape2")) {
    install.packages("reshape2")
}

require("data.table")
require("reshape2")

X_Train <- X_Test <- NULL
Y_Train <- Y_Test <- NULL
subject_Train <- NULL
subject_Test <- NULL

filePath <- function(...) {
    paste(..., sep = "/")
}

# download the data as is from the URL mentione below
downloadData <- function() {
    url <-
        "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    workingDir <- getwd()
    baseDir <- paste(workingDir,"data", sep = "/", collapse = "/")
    zipFile <- filePath(baseDir, "dataset.zip")
    if (!file.exists(zipFile)) {
        download.file(url, zipFile, method = "curl")
    }
    dataDir <- "UCI HAR Dataset"
    if (!file.exists(dataDir)) {
        unzip(zipFile, exdir = ".")
    }
    dataDir
}

# get the directory where the files are stored after unzipping
dataDir <- downloadData()

# Read the data after extracting it as per the above steps
readData <- function(path) {
    read.table(filePath(dataDir, path))
}

# read the training and test sets
if (is.null(X_Train)) {
    X_Train <<- readData("train/X_train.txt")
}

if (is.null(Y_Train)) {
    Y_Train <<- readData("train/Y_train.txt")
}

if (is.null(subject_Train)) {
    subject_Train <- readData("train/subject_train.txt")
}

if (is.null(X_Test))  {
    X_Test  <<- readData("test/X_test.txt")
}

if (is.null(Y_Test))  {
    Y_Test  <<- readData("test/Y_test.txt")
}

if (is.null(subject_Test))  {
    subject_Test <- readData("test/subject_test.txt")
}

featureNames <- readData("features.txt")[, 2]
activityNames <- readData("activity_labels.txt")[,2]

names(X_Test) = featureNames
X_Test = X_Test[,grepl("mean|std", featureNames)]

Y_Test[,2] = activityNames[Y_Test[,1]]
names(Y_Test) = c("Activity_ID", "Activity_Label")

names(subject_Test) = "Subject_ID"
test_data = cbind(as.data.table(subject_Test), Y_Test, X_Test)

names(X_Train) = featureNames
X_Train = X_Train[,grepl("mean|std", featureNames)]

Y_Train[,2] = activityNames[Y_Train[,1]]
names(Y_Train) = c("Activity_ID", "Activity_Label")

names(subject_Train) = "Subject_ID"
train_data = cbind(as.data.table(subject_Train), Y_Train, X_Train)

data = rbind(test_data, train_data)

id_labels   = c("Subject_ID", "Activity_ID", "Activity_Label")
data_labels = setdiff(colnames(data), id_labels)

melt_data      = melt(data, id = id_labels, measure.vars = data_labels)
tidy_data   = dcast(melt_data, Subject_ID + Activity_Label ~ variable, mean)

write.table(tidy_data, file = "tidy_data.txt")




