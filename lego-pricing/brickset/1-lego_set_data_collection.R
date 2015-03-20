# We're going to get the lists of sets from the brickset API
rm(list = ls())
# Here are the packages that you'll need: ----
library(XML)
library(stringr)
library(RCurl)
library(plyr)

# set initial parameters ----
api.url <- "http://brickset.com/api/v2.asmx/"
api.key <- "IDzN-C1HU-99ud"
uname <- "waketheman"
pword <- "serptin7"

getBricksetHashFromLogin <- function (api.url, api.key, uname, pword) {
  param.login <- "login?"
  params.key <- paste("apiKey=", api.key, sep = "")
  login.url <- paste(api.url, param.login, params.key, sep = "")
  params.uname <- paste("username=", uname, sep = "")
  params.pword <- paste("password=", pword, sep = "")
  login.string <- paste(login.url, params.uname, params.pword, sep = "&")
  u.hash.xml <- xmlInternalTreeParse(getURL(login.string))
  # login works!!
  # now extract the hash ----
  u.hash <- xmlValue(xmlRoot(u.hash.xml)[[1]])
  return(u.hash)
}

getBricksetSetList <- 
  function (api.url, api.key, u.hash, query = "", theme = "", subtheme = "",
            setNumber = "", year = "", owned = "", wanted = "", orderBy = "", 
            pageSize = 1, pageNumber = "", userName = "") {
    # testing:
    # query <- ""
    # theme <- ""
    # subtheme <- ""
    # setNumber <- ""
    # year <- 2014
    # owned <- ""
    # wanted <- ""
    # orderBy <- ""
    # pageSize <- ""
    # pageNumber <- ""
    # userName <- ""
    # here's what we need:
    param.setsList <- "getSets?"
    params.key <- paste("apiKey=", api.key, sep = "")
    setslist.uri <- paste(api.url, param.setsList, params.key, sep = "")
    params.uh <- paste("userHash=", u.hash, sep = "")
    params.query <- paste("query=", query, sep = "")
    params.theme <- paste("theme=", theme, sep = "")
    params.subtheme <- paste("subtheme=", subtheme, sep = "")
    params.setNumber <- paste("setNumber=", setNumber, sep = "")
    params.year <- paste("year=", year, sep = "")
    params.owned <- paste("owned=", owned, sep = "")
    params.wanted <- paste("wanted=", wanted, sep = "")
    params.orderBy <- paste("orderBy=", orderBy, sep = "")
    params.pageSize <- paste("pageSize=", pageSize, sep = "")
    params.pageNumber <- paste("pageNumber=", pageNumber, sep = "")
    params.userName <- paste("userName=", userName, sep = "")
    sets.string <- 
      paste(setslist.uri, params.uh, params.query, params.theme, params.subtheme, 
            params.setNumber, params.year, params.owned, params.wanted, 
            params.orderBy, params.pageSize, params.pageNumber, params.userName,
            sep = "&")
    # Now reach out to the api
    html.sets <- getURL(sets.string)
    # Now make it an XML document
    xml.sets <- xmlInternalTreeParse(html.sets)
    xml <- xmlRoot(xml.sets)
    # did they give us a list of sets?
    if (xmlSize(xml) > 0) {
      # Yes
      # try with xmlSApply
      sets.t <- xmlSApply(xml, function(x) xmlSApply(x, xmlValue))
      # it works but it's the wrong orientation
      sets <- as.data.frame(t(sets.t))
      row.names(sets) <- sets$setID
      # now convert all the factors into strings
      sets <- data.frame(lapply(sets, as.character), stringsAsFactors = FALSE)
    } else {
      # No - the list is empty
      sets <- FALSE
    }
    return (sets)
  }


getBricksetSetsByYear <- function (api.url, api.key, u.hash, year) {
  # assumes that there is at least one set in the request
  i <- 1
  # testing
  setsByYear <- NULL
  temp <- NULL
  # start with the first one
  setsByYear <- getBricksetSetList(api.url, api.key, u.hash, year, pageNumber = i)
  # if the function doesn't return false, there are more sets to get
  moreSets <- !is.logical(setsByYear)
  # keep going...
  while (moreSets) {
    i <- i + 1
    temp <- getBricksetSetList(api.url, api.key, u.hash, year, pageNumber = i)
    # if the function returns false, we've hit the end of the list
    if (is.logical(temp)) {
      moreSets <- FALSE
    } else {
      # Let's fix these so they have the same variables in the same order
      # are they different lengths?
      if (length(names(setsByYear)) > length(names(temp))) {
        # the temp list is shorter (assume it has a subset of the variables) 
        # in the setsByYear list
        colsToAdd <- setdiff(names(setsByYear), names(temp))
        for (j in 1:length(colsToAdd)) {
          # add another column, and set it's value to NA
          temp[1, ncol(temp) + 1] <- NA
          # name the last column in the df (the one just added)
          # with the name of the next item in the array
          names(temp)[ncol(temp)] <- colsToAdd[j]
        }
      }
      if (length(names(temp)) > length(names(setsByYear))) {
        # the setsByYear list is shorter (assume it has a subset of the variables) 
        # in the temp list
        colsToAdd <- setdiff(names(temp), names(setsByYear))
        for (k in 1:length(colsToAdd)) {
          # add another column, and set it's value to NA
          setsByYear[1, ncol(setsByYear) + 1] <- NA
          # name the last column in the df (the one just added)
          # with the name of the next item in the array
          names(setsByYear)[ncol(setsByYear)] <- colsToAdd[k]
        }
      }
      # now they are the same length
      # do they have elements in the same order?
      if (mean(as.numeric(names(setsByYear) == names(temp))) != 1) {
        # if not, put them in the same order
        temp <- temp[names(setsByYear)]
      }
      # now they have the same length, and are in the same order
      setsByYear <- rbind(setsByYear, temp)
    }
  }
  return(setsByYear)
}

# get the hash that we need for the api ----
u.hash <- getBricksetHashFromLogin(api.url, api.key, uname, pword)
rm(uname, pword)

# Now get a full set of data for 2014 - note, this will take a while... ----
year <- 2014
sets2014 <- getBricksetSetsByYear(api.url, api.key, u.hash, year = 2014)
# next for 2013
sets2013 <- getBricksetSetsByYear(api.url, api.key, u.hash, year = 2013)
# make sure the columns are in the right order
sets2013 <- sets2013[names(sets2014)]
# merge them
setsByYear <- rbind(sets2014, sets2013)
# clean it up
rm(sets2013, sets2014)
# now loop for the rest
for (y in seq(1969, 1960, -1)) {
  # get the set
  temp <- getBricksetSetsByYear(api.url, api.key, u.hash, year = y)
  # order the columns the same way as the current one
  temp <- temp[names(setsByYear)]
  # now merge them
  setsByYear <- rbind(setsByYear, temp)
  # hopefully this will set it up to update the environment with data...
  print(y)
}
table(setsByYear$year)
write.csv(setsByYear, "setsByYear.csv")
