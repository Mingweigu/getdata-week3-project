isWantedFeature <- function (x) {
  ## Determines if a feature name is a desired feature
  substrs <- c("mean()", "std()")
  any(sapply(substrs, function (y) grepl(y, x, fixed=TRUE)))
}

featureToColClass <- function(feature) {
  ## If this feature is desired, say the column class is numeric
  ## else return NULL to skip that column
  if (isWantedFeature(feature)) {
    "numeric"
  } else {
    "NULL"
  }
}

## Read in features from the dataset
features <- read.table(
  "UCI HAR Dataset/features.txt",
  col.names=c("id", "name"))


## Read in the map of activity ID to Name
activityMap <- read.table(
  "UCI HAR Dataset/activity_labels.txt",
  col.names=c("ActivityId", "ActivityName"))

## The classes of columns for the main data set
featureColClasses <- sapply(features$name, featureToColClass)


getDataset <- function (trainOrTest) {
  ## Given an argument "train" or "test"
  ## return the complete dataset, including X variables,
  ## Y variable (activity), and which subject ID was involved
  
  # Reads in the X variables, but only the columns
  # we are concerned about
  data <- read.table(
    paste("UCI HAR Dataset/", trainOrTest, "/X_",
          trainOrTest, ".txt", sep=""),
    col.names=features$name,
    colClasses=featureColClasses)
  
  # Reads in the subject data
  subjectData <- read.table(
    paste("UCI HAR Dataset/", trainOrTest,
          "/subject_", trainOrTest, ".txt", sep=""),
    col.names=c("SubjectId")
  )
  
  # Add subject data to the main data
  data$SubjectId <- subjectData$SubjectId
  
  # Read in Y variable data
  yData <- read.table(
    paste("UCI HAR Dataset/", trainOrTest,
          "/y_", trainOrTest, ".txt", sep=""),
    col.names=c("ActivityId")
  )
  
  # Replace ActivityID with ActivityName
  # and add to main data frame
  data$Activity <- merge(
    yData, activityMap)[,"ActivityName"]
  
  # Return main data frame
  data
}


## Get data for both the training set and test data
trainingData <- getDataset("train")
testData <- getDataset("test")


## Append this data together
allData <- rbind(testData, trainingData)

## Take the mean of all columns except SubjectId and Activity
## and output to tidyData
tidyData <- aggregate(
  allData[,!names(allData) %in% c("SubjectId", "Activity")],
  by=list(allData$SubjectId, allData$Activity), mean)

## Write tidyData to a file in the working directory
write.table(tidyData, file="tidyData.txt")
