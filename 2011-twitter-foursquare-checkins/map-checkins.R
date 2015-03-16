library(stringr)
library(maps)
library(plyr)

projectFolder = "Desktop/R/twitter-foursquare-checkins/"
projectGeoOutput = "wawa-geo-checkins.csv"
projectGeoFile = str_c(projectFolder,projectGeoOutput, sep = "")
projectStoresList = "wawastores.csv"
projectStoresOutput = "stores-updated.csv"
projectCheckinsOutput = "checkins aggregated.csv"

stores <- read.csv(str_c(projectFolder,projectStoresList, sep=""), header = TRUE, sep = ",", quote="\"'", dec = ".", fill = FALSE, comment.char="")
tweets <- read.csv(projectGeoFile, header = TRUE, sep = ",", quote="\"'", dec = ".", fill = FALSE, comment.char="")

# eliminate the ones without geo-coordinates
tweets <- subset(tweets,!is.na(longitude) & !is.na(latitude) &!is.na(venueTitle))
tweets$longitude <- round(as.numeric(tweets$longitude), digits = 2)
tweets$latitude <- round(as.numeric(tweets$latitude), digits = 2)
# set up the stores data:
stores <- subset(stores, (!is.na(stores$longitude) & !is.na(stores$latitude)))
stores$longitude <- round(as.numeric(stores$longitude), digits = 2)
stores$latitude <- round(as.numeric(stores$latitude), digits = 2)


# eliminate the ones that are clearly not in Wawa's geographic territory
tweets <- subset(tweets, (longitude < -69) & (latitude > 36) & (longitude > -86))
checkinDates <- tweets[order(-tweets$local.time),]

table(round(tweets$longitude, 0))
table(round(tweets$latitude, 0))

# merge checkins to stores:
aggCheckins <- ddply(tweets, .(longitude, latitude), "nrow")
storeCheckins <- merge(aggCheckins, stores, by=c("longitude","latitude"))
storeCheckins <- storeCheckins[order(-storeCheckins$nrow),] 
topStoreCheckins <- subset(storeCheckins, row(storeCheckins)<11)
topStoreCheckins <- subset(topStoreCheckins, !is.na(topStoreCheckins$longitude))
#trying with the maps package
# plot the US
# map("state", interior = FALSE)
# plot the states
# data(world.cities)
# myCities <- subset(world.cities, (lat < 41) & (lat > 36) & (long > -78) & (long < -73))
col2rgb("black")
col2rgb("yellow")
# wawa's orange is 219,154,48
# wawa's red is 209,15,40
statesList = c('new jersey', 'd.c.', 'penn', 'delaware', 'maryland', 'virginia')
# statesList = c('penn')
# Wawa tan color is 219,154,48
# Wawa red color is 209,15,40
map("state", fill=FALSE, region = statesList, col=rgb(219,154,48,255,maxColorValue=255), resolution = 0)
points(storeCheckins$longitude, storeCheckins$latitude, type="p", pch = 16, col = rgb(0,0,0,150,maxColorValue=255), cex = storeCheckins$nrow/10)
points(topStoreCheckins$longitude, topStoreCheckins$latitude, type="p", pch = 16, col = rgb(209,15,40,255,maxColorValue=255), cex = topStoreCheckins$nrow/10)

# points(myCities$long,myCities$lat, type="p", pch = 16, col = rgb(0,0,0,200,maxColorValue=255), cex = 1)
# points(stores$longitude,stores$latitude, type="p", pch = 16, col = rgb(0,0,0,255,maxColorValue=255), cex = .9)

# time with time
years <- as.numeric(str_sub(tweets$local.time, start=1, end=4))
months <- as.numeric(str_sub(tweets$local.time, start=6, end=7))
days <- as.numeric(str_sub(tweets$local.time, start=9, end=10))
hours <- as.numeric(str_sub(tweetData$local.time, start=12, end=13))
minutes <- as.numeric(str_sub(tweets$local.time, start=15, end=16))
seconds <- as.numeric(str_sub(tweets$local.time, start=18, end=19))

table(hours)


# write.table(topStoreCheckins, str_c(projectFolder,"top stores for checkins.csv", sep=""), append = FALSE, quote = TRUE, sep = ",", eol = "\r", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")