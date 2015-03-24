# this is for getting the lists of parts
library(XML)
library(stringr)

# The next step will be to actually build a parts list from the set list. That will be a lot more useful...
partsList <- NULL
for(i in 1:nrow(brickSetsPriced)) {
  # Now we can test the new function
  # if(brickSetsPriced$pieces[i] > 2) {
    tempPartsList <- dfr_peeronPartsList(brickSetsPriced$legoID[i])
    if(length(tempPartsList) > 1) {
      partsList <- rbind(tempPartsList, partsList)
    }
    print(paste("Getting set in row", i))
  # }
}

write.csv(partsList, "partsList.csv")
