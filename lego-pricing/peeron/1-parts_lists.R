# this is for getting the lists of parts

# The next step will be to actually build a parts list from the set list. That will be a lot more useful...
partsList <- NULL
for(i in nrow(setsList):1) {
  # Now we can test the new function
  if(setsList$Pcs[i] > 2) {
    tempPartsList <- dfr_peeronPartsList(setsList$SetID[i])
    partsList <- rbind(tempPartsList, partsList)
  }
}

