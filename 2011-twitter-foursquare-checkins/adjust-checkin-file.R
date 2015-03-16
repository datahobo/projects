library(XML)
library(stringr)

firstRun <- FALSE
projectFolder = "/Users/waketheman/Desktop/R/twitter-foursquare-checkins/"
projectData = "tweets.csv"
projectWawa = "wawa-checkins.csv"
projectDataFile = str_c(projectFolder,projectData, sep = "")
projectWawaFile = str_c(projectFolder,projectWawa, sep = "")


if(!firstRun) tweetData <- read.csv(projectDataFile, header = TRUE, sep = ",", quote="\"'", dec = ".", fill = FALSE, comment.char="")

wawa4SqCheckins <- subset(tweetData,tweetData$brandName=="Wawa")


write.table(wawa4SqCheckins, projectWawaFile, append = FALSE, quote = TRUE, sep = ",", eol = "\r", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")


tweetData$year <- str_sub(tweetData$local.time, start=1, end=4)
tweetData$month <- str_sub(tweetData$local.time, start=6, end=7)
tweetData$day <- str_sub(tweetData$local.time, start=9, end=10)
tweetData$hour <- str_sub(tweetData$local.time, start=12, end=13)
tweetData$minute <- str_sub(tweetData$local.time, start=15, end=16)
tweetData$second <- str_sub(tweetData$local.time, start=18, end=19)

table(tweetData$year)
table(tweetData$month)
table(tweetData$day)
table(tweetData$hour)
table(tweetData$minute)
table(tweetData$second)

table(tweetData$month,tweetData$day)

names(tweetData)
