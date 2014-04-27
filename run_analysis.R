isWantedFeature <- function (x) {
  substrs <- c("mean()", "std()")
  any(sapply(substrs, function (y) grepl(y, x, fixed=TRUE)))
}

featureToColClass <- function(feature) {
  if (isWantedFeature(feature)) {
    "numeric"
  } else {
    "NULL"
  }
}

features <- read.table(
  "UCI HAR Dataset/features.txt",
  col.names=c("id", "name"))

activityMap <- read.table(
  "UCI HAR Dataset/activity_labels.txt",
  col.names=c("ActivityId", "ActivityName"))

featureColClasses <- sapply(features$name, featureToColClass)

getDataset <- function (trainOrTest) {
  data <- read.table(
    paste("UCI HAR Dataset/", trainOrTest, "/X_",
          trainOrTest, ".txt", sep=""),
    col.names=features$name,
    colClasses=featureColClasses)
  
  subjectData <- read.table(
    paste("UCI HAR Dataset/", trainOrTest,
          "/subject_", trainOrTest, ".txt", sep=""),
    col.names=c("SubjectId")
  )
  
  data$SubjectId <- subjectData$SubjectId
  
  yData <- read.table(
    paste("UCI HAR Dataset/", trainOrTest,
          "/y_", trainOrTest, ".txt", sep=""),
    col.names=c("ActivityId")
  )
  
  data$Activity <- merge(
    yData, activityMap)[,"ActivityName"]
  
  data
}

trainingData <- getDataset("train")
testData <- getDataset("test")

allData <- rbind(testData, trainingData)

tidyData <- aggregate(
  allData[,!names(allData) %in% c("SubjectId", "Activity")],
  by=list(allData$SubjectId, allData$Activity), mean)

write.table(tidyData, file="tidyData.txt")
