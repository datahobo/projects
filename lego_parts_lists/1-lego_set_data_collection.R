# We're going to get all the sets from brickset
# And all the parts lists for all the lego sets from the site rebrickable.com
# It's all API-based

# Here are the packages that you'll need:
library(XML)
library(stringr)
library(RCurl)

# A few core items:
bsAPIKey <- 'IDzN-C1HU-99ud'
bsUserName <- 'waketheman'
bsPassWord <- 'serptin7'
rbAPIKey <- '8m00WX6eyX'

bsURLBase <- 'http://brickset.com/webservices/brickset.asmx'
# v2 of the brickset API:
# bricksetURLBase <- 'http://brickset.com/api/v2.asmx'

rbURLBase <- 'http://rebrickable.com/api/'

getRBPartsList <- function (setID) {
  # Start with our core string elements
  rbSetURL <- str_c(rbURLBase, 'get_set_parts', sep = "", collapse = NULL)
  rbAPIParam <- str_c('?key=', rbAPIKey, sep = "", collapse = NULL)
  rbSetParam <- str_c('&set=', setID, sep = "", collapse = NULL)
  rbFormatParam <- '&format=xml'
  
  rbSetURL <- str_c(rbSetURL, rbAPIParam, rbSetParam, rbFormatParam, sep = "",
                    collapse = NULL)
  # Now get the list from RB
  temp <- getURL(rbSetURL)
  # Stick it in an XML doc
  XML <- xmlParse(temp)
  # Extract the list of "part" nodes
  parts <- getNodeSet(XML, "//part")
  # Some initial counters
  partsIndexed <- 0
  partsCount <- length(parts)
  # Placeholder for the data frame
  partsList <- NULL
  readMoreParts <- ((partsCount - partsIndexed) > 0)
  if (readMoreParts) {
    for (i in 1:partsCount) {
      # put the part in a separate object to manipulate
      partXML <- xmlDoc(parts[[i]])
      # Now pull out the elements we need, in the right formats
      partID <- as.character(unlist(xpathApply(partXML, "//part_id", xmlValue)))
      quantity <- as.numeric(unlist(xpathApply(partXML, "//qty", xmlValue)))
      ldrawColorID <- as.numeric(unlist(xpathApply(partXML, "//ldraw_color_id",
                                                   xmlValue)))
      type <- as.numeric(unlist(xpathApply(partXML, "//type", xmlValue)))
      partName <- as.character(unlist(xpathApply(partXML, "//part_name",
                                                 xmlValue)))
      colorName <- as.character(unlist(xpathApply(partXML, "//color_name",
                                                  xmlValue)))
      partImageURL <- as.character(unlist(xpathApply(partXML, "//part_img_url",
                                                     xmlValue)))
      elementID <- as.numeric(unlist(xpathApply(partXML, "//element_id",
                                                xmlValue)))
      elementImageURL <- as.character(unlist(xpathApply(partXML,
                                                        "//element_img_url",
                                                        xmlValue)))
      # Create an individual data frame
      partFrame <- data.frame(partID, quantity, ldrawColorID, type, partName,
                              colorName, partImageURL, elementID, elementImageURL)
      # Increment the parts counter
      partsIndexed <- partsIndexed + 1
      # Use it for the identifier in the data frame
      # rownames(partFrame) <- partsIndexed
      # Now pull it back into the main list
      partsList <- rbind(partsList, partFrame)
    }
    # We need to confirm that there are more parts to index
    readMoreParts <- ((partsCount - partsIndexed) > 0)
  }
  # The final data frame has all of the parts
  partsList$setID <- setID
  return(partsList)
}

getBSUserHash <- function (bsUser, bsPass) {
  bsLogin <- '/login?'
  bsUser <- str_c('u=', bsUser, sep = "", collapse = NULL)
  bsPass <- str_c('&p=', bsPass, sep = "", collapse = NULL)  
  # Now build a temp string
  temp <- str_c(bsURLBase, bsLogin, bsUser, bsPass, sep = "",
                collapse = NULL)
  bsUserHash <- getURL(temp)
  # can't figure out how to manipulate it as XML, so I'll do it as a string
  bsUserHash <- str_extract(bsUserHash, '>.*<')
  bsUserHash <- str_sub(bsUserHash, start = 2L, end = -1L)
  bsUserHash <- str_extract(bsUserHash, '>.*<')
  bsUserHash <- str_sub(bsUserHash, start = 2L, end = -2L)
  return(bsUserHash)
}

getBSSetListXMLByYear <- function (bsAPIKey, bsUserHash, bsYear) {
  # This function searches for a specific year, and returns a data frame
  # of all the sets in that year
  bsSearch <- '/search?'  
  bsAPI <- str_c('apiKey=', bsAPIKey, sep = "", collapse = NULL)
  bsUserHash <- str_c('&userHash=', bsUserHash, sep = "", collapse = NULL)
  bsQuery <- '&query='
  bsTheme <- '&theme='
  bsSubTheme <- '&subtheme='
  bsSetNumber <- '&setNumber='
  bsOwned <- '&Owned='
  bsWanted <- '&Wanted='
  bsYear <- str_c('&year=', as.character(bsYear), sep = "", collapse = NULL)
  temp <- str_c(bsURLBase, bsSearch, bsAPI, bsUserHash, bsQuery, bsTheme,
                bsSubTheme, bsSetNumber, bsYear, bsOwned, bsWanted, 
                sep = "", collapse = NULL)
  bsSetListByYear <- getURL(temp)
  # Now that we have a string, we'll need to convert it to an XML node/tree
  bsSetListByYear <- xmlParse(bsSetListByYear)
  bsSetListByYear <- getNodeSet(bsSetListByYear, "//r:setData",
                     namespaces = c(r = 'http://brickset.com/webServices/'))
  return(bsSetListByYear)
}

getBSSetListFromXML <- function (bsSetListXML) {
  # We're starting with XML
  setsCount <- length(bsSetListXML)
  setsIndexed <- 0
  setsList <- NULL
  readMoreSets <- ((setsCount - setsIndexed) > 0)
  if (readMoreSets) {
    for (i in 1:setsCount) {
      # put the set into a separate object to manipulate
      setXML <- xmlDoc(bsSetListXML[[i]])
      # Now pull out the elements we need, in the right formats
      bsID <- unlist(xpathApply(setXML, "//r:number", xmlValue,
                                namespaces = c(r = 'http://brickset.com/webServices/')))
      bsIDV <- unlist(xpathApply(setXML, "//r:numberVariant", xmlValue,
                                 namespaces = c(r = 'http://brickset.com/webServices/')))
      setID <- str_c(bsID, bsIDV, sep = "-", collapse = NULL)
      setName <- unlist(xpathApply(setXML, "//r:setName", xmlValue, 
                                   namespaces = c(r = 'http://brickset.com/webServices/')))
      year <- unlist(xpathApply(setXML, "//r:year", xmlValue,
                                namespaces = c(r = 'http://brickset.com/webServices/')))
      theme <- unlist(xpathApply(setXML, "//r:theme", xmlValue,
                                 namespaces = c(r = 'http://brickset.com/webServices/')))
      subTheme <- unlist(xpathApply(setXML, "//r:subtheme", xmlValue,
                                    namespaces = c(r = 'http://brickset.com/webServices/')))
      pieces <- unlist(xpathApply(setXML, "//r:pieces", xmlValue,
                                  namespaces = c(r = 'http://brickset.com/webServices/')))
      minifigs <- unlist(xpathApply(setXML, "//r:minifigs", xmlValue,
                                    namespaces = c(r = 'http://brickset.com/webServices/')))
      bsURL <- unlist(xpathApply(setXML, "//r:bricksetURL", xmlValue,
                                 namespaces = c(r = 'http://brickset.com/webServices/')))
      price <- unlist(xpathApply(setXML, "//r:USRetailPrice", xmlValue,
                                 namespaces = c(r = 'http://brickset.com/webServices/')))
      instructions <- unlist(xpathApply(setXML, "//r:instructionsAvailable", xmlValue,
                                        namespaces = c(r = 'http://brickset.com/webServices/')))
      updated <- unlist(xpathApply(setXML, "//r:lastUpdated", xmlValue,
                                   namespaces = c(r = 'http://brickset.com/webServices/')))


      # Add it to a data frame
      setFrame <- data.frame(setID, setName, year, theme, subTheme, pieces, minifigs,
                             bsURL, price, instructions, updated, stringsAsFactors = FALSE)
      # Increment the sets counter
      setsIndexed <- setsIndexed + 1
      # Use it for the identifier in the data frame
      rownames(setFrame) <- setsIndexed
      # Now pull it back into the main list
      setsList <- rbind(setsList, setFrame)
    }
    readMoreSets <- ((setsCount - setsIndexed) > 0)
  }
  return(setsList)
}

bsUserHash <- getBSUserHash(bsUserName, bsPassWord)
# bsSetList2014 <- getBSSetListXMLByYear(bsAPIKey, bsUserHash, '2014')
# bsSetList2014 <- getBSSetListFromXML(bsSetList2014)
bsSetList <- NULL
for(year in 1980:2014) {
  bsSetListByYear <- getBSSetListXMLByYear(bsAPIKey, bsUserHash, as.character(year))
  bsSetListByYear <- getBSSetListFromXML(bsSetListByYear)
  bsSetList <- rbind(bsSetList, bsSetListByYear)
}

# Clean up the numeric fields in the table
bsSetList$Price <- as.numeric(bsSetList$price)
bsSetList$Year <- as.numeric(bsSetList$year)
bsSetList$Pieces <- as.numeric(bsSetList$pieces)
bsSetList$Minifigs <- as.numeric(bsSetList$minifigs)
# Get rid of the extra fields
bsSetList$price <- NULL
bsSetList$year <- NULL
bsSetList$pieces <- NULL
bsSetList$minifigs <- NULL

appropriateSets <- subset(bsSetList, !(is.na(bsSetList$Price))
                          & !(is.na(bsSetList$Pieces)) & !(is.na(bsSetList$Year))
                          & bsSetList$theme != 'Duplo' & bsSetList$theme != 'Belville'
                          & bsSetList$theme != 'Clikits' & bsSetList$theme != 'Dacta'
                          )
# let's figure out which themes are the most popular
aSetsThemes <- table(appropriateSets$theme)
# now re-order them in descending order
aSetsThemes <- aSetsThemes[order(-aSetsThemes)]
aSetsThemes

partsList <- NULL
# for(i in nrow(setListWithPrices):1) {
for(i in 100:1) {
  rbPartsListBySet <- getRBPartsList(appropriateSets$setID[i])
  partsList <- rbind(rbPartsListBySet, partsList)
}
