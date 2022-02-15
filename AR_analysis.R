subject_numbers <- c(6:39,41,42,46,47,49:55,57:59)
#combine data across subjects
all_data = c();
for (i in subject_numbers) {
  filename<-paste0("SJ",i,".csv")
  thisData <- read.csv(filename)
  subject_id <- matrix(i, nrow(thisData), 1)
  thisData <- cbind(subject_id, thisData)
  all_data <- rbind(all_data,thisData)
}

sessions <- 0:5
nRowsPerSub <- c();
finalCount <- c();
for (subjectIdx in subject_numbers) {
  for (sessionIdx in sessions){
    sessionData <- c();
    allrows <- nrow(all_data)
    for (val in 1:allrows) { 
      if (all_data[val,1]==subjectIdx && all_data[val,3]==sessionIdx) {
        sessionData <- rbind(sessionData,all_data[val,])
      }
    }
    #filter headposz: -1.50 to -18.50 and headposx: 6:50 to 18:50
    sessionLength <- nrow(sessionData)
    nRows <- 0
    count <- 0
    for (rowNum in 1:sessionLength){
      if (sessionData[rowNum,7] <= -1.50 && sessionData[rowNum,7] >= -18.5 && sessionData[rowNum,5] >= 6.50 && sessionData[rowNum,5] <= 18.5){
        nRows <- nRows + 1
        check <- rowNum
      }
    }
    thisCount <- cbind(subjectIdx,sessionIdx,nRows)
    finalCount <- rbind(finalCount, thisCount)
  }
}

write.csv(x = finalCount, file = "rowCounts")
