# We're going to get all the parts lists for all the lego sets from the site peeron.com
# and the parts list site: http://www.peeron.com/inv/sets/852001-1

# Here are the packages that you'll need:
library(XML)
library(stringr)

# Here are the functions that you get:
  # str_peeronPartsListURL creates a URL for a specific set's page of parts on Peeron
  # dfr_cleanPeeronPartsList cleans the list of parts for  aset
  # dfr_peeronPartsList creates a dataframe for a parts list, uses dfr_cleanPeeronPartsList

# Some functions that we'll need to create URLs:
# Let's use paste because it's part of the base package
str_peeronPartsListURL <- function (setID) {
# This is to get the list of parts associated with a set
  urlString <- "http://www.peeron.com/inv/sets/"
  urlString <- paste(urlString, setID, sep = "", collapse = NULL)
  return(urlString)
}

# Now we'll create some functions to re-cast the columns in our parts list....
dfr_cleanPeeronPartsList <- function (partsList) {
  # First we need the right headers...
  tempHeader <- colnames(partsList)
  tempHeader <- str_replace_all(tempHeader, "NULL", "")
  tempHeader <- str_replace_all(tempHeader, "[.]", "")
  colnames(partsList) <- tempHeader

  # This works with a data frame coming out of the peeron import
  partsList$Qty <- as.numeric(levels(partsList$Qty))[partsList$Qty]
  partsList$PartNum <- as.character(partsList$PartNum)
  partsList$Color <- as.character(partsList$Color)
  partsList$Description <- as.character(partsList$Description)
  partsList$Note <- as.character(partsList$Note)
  
  return(partsList)
}

# Let's create a function to get all the parts from a parts list
dfr_peeronPartsList <- function (setIDString) {
  # print(paste("Running", setIDString))
  # Define the URL we're going to get
  setURL <- str_peeronPartsListURL(setIDString)
  # Now pull the page into an R object
  setPartsListPage <- readHTMLTable(setURL)  
  # Convert it to a data frame - the last table in the list should be what we want
  setPartsList <- as.data.frame(setPartsListPage[length(setPartsListPage)])
  # if it's a valid setAdd the setNumber to the data frame
  validSet <- (length(setPartsList) > 1 &
                 names(setPartsList)[1] != "NULL.V1")
  if(validSet) {
    setPartsList$SetID <- setIDString  
    # Use the function to clean the data frame
    setPartsList <- dfr_cleanPeeronPartsList(setPartsList)
    # print(paste("Cleaned", setIDString))
    return(setPartsList)
  } else {
    return(NA)
  }
}

# Let's use paste because it's part of the base package
str_peeronSetListURL <- function (page) {
  # This is for the list of sets available on Peeron
  urlString <- 'http://www.peeron.com/cgi-bin/invcgis/setsearch?Inv=YES&PagerSortDir=r&PagerSortCol=MSRP&PagerSortRev=1'
  urlString <- paste(urlString, "&PagerPage=", as.character(page), sep = "", collapse = NULL)
  return(urlString)
}
# Now we'll create some functions to re-cast the columns in each of our data frames...
dfr_cleanPeeronSetList <- function (setList) {
  # First we need to fix the column names
  # Make sure you load stringr!
  tempHeader <- colnames(setList)
  tempHeader <- str_replace_all(tempHeader, "NULL", "")
  tempHeader <- str_replace_all(tempHeader, "[.]", "")
  colnames(setList) <- tempHeader
  
  # Now we can update the types...
  # This works with a dataframe coming out of the peeron import
  setList$SetID <- as.character(setList$SetID)
  setList$Name <- as.character(setList$Name)
  setList$Theme <- as.character(setList$Theme)
  # Ahhh....factors
  setList$Year <- as.numeric(levels(setList$Year))[setList$Year]
  setList$Pcs <- as.numeric(levels(setList$Pcs))[setList$Pcs]
  setList$Figs <- as.numeric(levels(setList$Figs))[setList$Figs]
  # We'll do a little more work with this one
  setList$MSRP <- as.character(setList$MSRP)
  setList$MSRP <- str_replace_all(setList$MSRP, '\\$','')
  setList$MSRP <- as.numeric(setList$MSRP)
  setList$Inventory <- as.character(setList$Inventory)
  setList$Instructions <- as.character(setList$Instructions)
  
  return(setList)  
}
# Now we move onto the interesting part, grabbing the list of all the sets...
peeronSets <- NULL
for(i in 1:88) {
  # for(i in 1:1) {
  # There are 48 pages of sets that have prices
  # First we get a URL
  listURL <- str_peeronSetListURL(i)
  # Then download that page
  listPage <- readHTMLTable(listURL, header = TRUE)
  # Put the right table into a dataframe
  peeronSetsList <- NULL
  peeronSetsList <- as.data.frame(listPage[2])
  # Now clean up the table
  peeronSetsList <- dfr_cleanPeeronSetList(peeronSetsList)
  # Now all we need is a merging function...
  peeronSets <- rbind(peeronSets, peeronSetsList)
}
rm(i, listURL, listPage, peeronSetsList)

write.csv(peeronSets, "peeronSets.csv")
