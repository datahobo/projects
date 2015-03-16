library(XML)
library(stringr)

firstRun <- TRUE
projectFolder = "/Users/waketheman/Documents/My Documents/active projects/R"
projectData = "tweets.csv"
projectDataFile = str_c(projectFolder,projectData, sep = "")

if(!firstRun) tweetData <- read.csv(projectDataFile, header = TRUE, sep = ",", quote="\"'", dec = ".", fill = FALSE, comment.char="")

brands = c("Wawa", "Sheetz", "Dunkin", "7-Eleven")
brand.index = 1
tweets <- NULL
tweet.count <- 0

# loop through for every brand being tracked
while (brand.index < length(brands) + 1)
{
    # setup the search string
  	brandName <- brands[brand.index]
  	brandFilter <- brandName
   
    if(!firstRun)
    {
        # set the starting tweet for the brand
        tempTweets <- subset(tweetData, brandName == brandFilter)
        if (is.na(tempTweets$username[1])) firstMessage <- 0 else {
            firstMessage <- max(tempTweets$messageID)
        }
        rm(tempTweets)
    }
  
  	twitter.checkins.search <- str_c(brandName, " ", "foursquare")
  
  	QUERY <- URLencode(twitter.checkins.search)
    if(!firstRun) {
        if (firstMessage>0) QUERY <- str_c(QUERY,'&since_id=', as.character(firstMessage))
    }
    page <- 1
  	read.more <- TRUE
  
  	while (read.more)
  	{
		# construct Twitter search URL
		URL <- paste('http://search.twitter.com/search.atom?q=',QUERY,'&rpp=100&page=', page, sep='')
		# fetch remote URL and parse
		XML <- htmlTreeParse(URL, useInternal=TRUE)
		
		# Extract list of "entry" nodes
		entry     <- getNodeSet(XML, "//entry")
		
		read.more <- (length(entry) > 0)
		if (read.more)
		{
  			for (i in 1:length(entry))
  			{
				subdoc     <- xmlDoc(entry[[i]])   # put entry in separate object to manipulate

				# put the tweet in
				message  <- unlist(xpathApply(subdoc, "//title", xmlValue))
        message <- str_replace_all(message,'"','')
            
				# let's find everything we don't want
				isReTweet <- str_detect(message, "RT @")
				isMayorMessage <- str_detect(message, "@foursquare")
				is4sqVenue <- str_detect(message, "4sqVenue")
				skipMessage <- any(isReTweet,isMayorMessage, is4sqVenue)

				if(!skipMessage)
				{
  					published  <- unlist(xpathApply(subdoc, "//published", xmlValue))
  					# see Converting time zones in R: tips, tricks and pitfalls
  					# http://blog.revolutionanalytics.com/2009/06/converting-time-zones.html
  					# Example "published" string:  "2011-05-19T00:57:41Z"
  					# Let's remove "T" and "Z" from string
  					published  <- gsub("Z"," ", gsub("T"," ",published) )
  					# Convert from GMT to eastern time
  					time.gmt   <- as.POSIXct(published,"GMT")
  					local.time <- format(time.gmt, tz="America/New_York")
  					# local.time <- strptime(local.time, "%Y-%m-%d %H:%M:%S")
  	
  					# get the messageID
  					messageID <- unlist(xpathApply(subdoc,"//id", xmlValue))
  					messageID <- as.numeric(str_extract(messageID[1], "[0-9]{6,}"))
  
  					# now the user
  					author <- unlist(xpathApply(subdoc, "//author/name",  xmlValue))
  					username <- unlist(xpathApply(subdoc, "//author/uri", xmlValue))
  					# what we really want:
  					username <- str_sub(username, start=20)
  					person <- str_sub(author,start = 3 + str_length(username), end = -2)
  
  					# now for the foursquare URL:
  					checkinURL <- str_extract(message, "http://t.*")[1]
  
            # if the checkinURL is NOT blank...
            if (!is.na(checkinURL))
            {
        				# now there might be a problem with the URL:
      					if (str_detect(checkinURL, " "))
      					{
    				  		breakURL <- str_locate(checkinURL, " ")[1] - 1
    					  	checkinURL <- str_sub(checkinURL, 1, breakURL)
      					}
              
                entry.frame <- data.frame(local.time, username, person, message, messageID, checkinURL, brandName)
                        tweet.count <- tweet.count + 1
                  		rownames(entry.frame) <- tweet.count
    	    			tweets <- rbind(tweets, entry.frame)
                    }
            }
    	}
			page <- page + 1
  			read.more <- (page <= 15)   # Seems to be 15 page limit
		}
	}
  brand.index <- brand.index + 1
}

if(!firstRun)  tweetData <- rbind(tweetData, tweets) else tweetData <- tweets
write.table(tweetData, projectDataFile, append = FALSE, quote = TRUE, sep = ",", eol = "\r", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")
table(tweetData$brandName)