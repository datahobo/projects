library("maps")
library("plyr")
library("stringr")
library("zipcode")
Wawa.Insider.Facebook.Tab...OptIns...Aug.31.2012.10.02.26.AM <- read.csv("~/Desktop/Wawa Insider Facebook Tab - OptIns - Aug 31 2012 10-02-26 AM.CSV")
View(Wawa.Insider.Facebook.Tab...OptIns...Aug.31.2012.10.02.26.AM)

Wawa.Insider.Facebook.Tab...OptIns...Aug.31.2012.10.02.26.AM$ZipCodes <- str_sub(Wawa.Insider.Facebook.Tab...OptIns...Aug.31.2012.10.02.26.AM$Zip,1,5)
Wawa.Insider.Zipcodes <- ddply(Wawa.Insider.Facebook.Tab...OptIns...Aug.31.2012.10.02.26.AM, .(ZipCodes), "nrow")
View(Wawa.Insider.Zipcodes)
Wawa.Insider.Zipcodes$zip <- Wawa.Insider.Zipcodes$ZipCodes
View(Wawa.Insider.Zipcodes)

View(zipcode)

GeoZipCodes <- merge(Wawa.Insider.Zipcodes, zipcode,by="zip")

# map(database="usa")
statesList = c('florida')
# Wawa tan color is 219,154,48
# Wawa red color is 209,15,40
map("state", fill=TRUE, region = statesList, col=rgb(219,154,48,255,maxColorValue=255), resolution = 0)
points(GeoZipCodes$longitude, GeoZipCodes$latitude, type="p", pch = 16, col = rgb(0,0,0,150,maxColorValue=255), cex = GeoZipCodes$nrow/10)

points(storeCheckins$longitude, storeCheckins$latitude, type="p", pch = 16, col = rgb(0,0,0,150,maxColorValue=255), cex = storeCheckins$nrow/10)
points(topStoreCheckins$longitude, topStoreCheckins$latitude, type="p", pch = 16, col = rgb(209,15,40,200,maxColorValue=255), cex = topStoreCheckins$nrow/10)
points(stores$longitude, stores$latitude, type="p", pch = 16, col = rgb(0,0,0,150,maxColorValue=255), cex = 1)
