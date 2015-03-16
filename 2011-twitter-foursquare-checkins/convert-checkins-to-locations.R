library(stringr)
library(RCurl)
options(RCurlOptions = list(followlocation = TRUE, useragent = "R"))
# timeout = 1000,

projectFolder = "/Users/waketheman/Desktop/R/twitter-foursquare-checkins/"
projectData = "tweets.csv"
projectDataFile = str_c(projectFolder,projectData, sep = "")
projectGeoOutput = "wawa-geo-checkins.csv"
projectGeoFile = str_c(projectFolder,projectGeoOutput, sep = "")

tweets <- read.csv(projectDataFile, header = TRUE, sep = ",", quote="\"'", dec = ".", fill = FALSE, comment.char="")
tweetData <- subset(tweets, brandName == "Wawa")
rm(tweets)

for(i in max(row(tweetData)):1)
# for(i in 1:100)
# (i = 1)
{
    #convert the checkin to the venue
    webpage <- getURL(tweetData$checkinURL[i])
    if(!any(is.na(webpage),str_detect(webpage,"Not Found")))
    {
        # convert to a foursquare venue
        webpage <- str_sub(webpage, start = str_locate(webpage, "/v/")[1] + 3)
        webpage <- str_sub(webpage, start = str_locate(webpage, "/")[1] + 1)
        endVenue <- str_locate(webpage, ">")[1] - 2
        venue4sqID <- str_sub(webpage, end = endVenue)
        
        # assuming we've found a foursquare venue page
        if(!(is.na(venue4sqID)))
        {
            webpage <- getURL(str_c("https://foursquare.com/v/",venue4sqID))
  		
            # get its longitude
            startLongitude <- str_locate(webpage, "og:longitude")[1] - 40
            collectLongitude <- str_sub(webpage, start=startLongitude, end=startLongitude+40)
            startLongitude <- str_locate(collectLongitude, "=")[1] + 2
            collectLongitude <- str_sub(collectLongitude, start=startLongitude)
            endLongitude <- str_locate(collectLongitude,"\"")[2] - 1
            longitude <- str_sub(collectLongitude, end=endLongitude)
              
            # get its latitude
            startLatitude <- str_locate(webpage, "og:latitude")[1] - 40
            collectLatitude <- str_sub(webpage, start=startLatitude, end=startLatitude+40)
            startLatitude <- str_locate(collectLatitude, "=")[1] + 2
            collectLatitude <- str_sub(collectLatitude, start=startLatitude)
            endLatitude <- str_locate(collectLatitude,"\"")[2] - 1
            latitude <- str_sub(collectLatitude, end=endLatitude)
            
            # get the foursquare name of the site
            startTitle <- str_locate(webpage, "og:title")[1] - 50
            collectTitle <- str_sub(webpage, start=startTitle, end=startTitle+50)
            startTitle <- str_locate(collectTitle, "=")[1] + 2
            collectTitle <- str_sub(collectTitle, start=startTitle)
            endTitle <- str_locate(collectTitle,"\"")[2] - 1
            venueTitle <- str_sub(collectTitle, end=endTitle)
            
            tweetData$venue4sqID[i] <- venue4sqID
            tweetData$longitude[i] <- longitude
            tweetData$latitude[i] <- latitude
            tweetData$venueTitle[i] <- venueTitle
            Sys.sleep(2)
        }
    }
}
write.table(tweetData, projectGeoFile, append = FALSE, quote = TRUE, sep = ",", eol = "\r", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")
