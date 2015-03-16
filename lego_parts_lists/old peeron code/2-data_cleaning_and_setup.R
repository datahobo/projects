# Here are the packages that you'll need:
library(XML)
library(stringr)
library(plyr)
library(car)
# Here are the datasets you'll use:
# workingSetsList
# workingPartsList

 workingSetsList <- setsList
# workingPartsList <- partsList

# Let's figure out if there are any weird set IDs
setIDsList <- setsList$SetID
setIDsSplit <- str_split(setIDsList, "-")
setIDSuffix <- NULL
setIDNumber <- NULL
for(i in length(setIDsSplit):1) {
  setIDSuffix[i] <- as.numeric(setIDsSplit[[i]][2])
  setIDNumber[i] <- as.numeric(setIDsSplit[[i]][1])
}
max(setIDSuffix)
table(setIDSuffix)
summary(setIDNumber)
table(setIDNumber)
# Unfortunately there are 120 sets with a suffix >1

# Let's work on some categorization of the sets.
setCategories <- workingSetsList$Theme
setCategoryHierarchy <- str_split(setCategories, " / ")
# Check to make sure everything made it in
length(setCategoryHierarchy)

# What's the first one?
length(setCategoryHierarchy[[1]])
setCategoryLevels <- NULL
for(i in length(setCategoryHierarchy):1) {
  setCategoryLevels[i] <- length(setCategoryHierarchy[[i]])
}
max(setCategoryLevels)
# So now we know that there are at most 5 category levels
# Let's make this into a data.frame with the set ID.

setCategoryHierarchy[[1]][5]
setCategoryStructure <- NULL
for(i in length(setCategoryHierarchy):1) {
  setCategoryStructure$Primary[i] <- setCategoryHierarchy[[i]][1]
  setCategoryStructure$Secondary[i] <- setCategoryHierarchy[[i]][2]
  setCategoryStructure$Tertiary[i] <- setCategoryHierarchy[[i]][3]
  setCategoryStructure$Fourth[i] <- setCategoryHierarchy[[i]][4]
  setCategoryStructure$Fifth[i] <- setCategoryHierarchy[[i]][5]
  setCategoryStructure$Sixth[i] <- setCategoryHierarchy[[i]][6] 
}

setCategoryData <- NULL
setCategories <- NULL
setCategoryData <- as.data.frame(setCategoryStructure)
setCategories$SetID <- workingSetsList$SetID
setCategories <- cbind(setCategories, setCategoryData)
setCategories <- as.data.frame(setCategories)
# Now that we have a data frame
summary(setCategories)
table(setCategories$Secondary)

# Let's get it back with our sets information.
setsAndCategories <- merge(setsList, setCategories)
# So there are a few datasets we don't need anymore
rm(setsList, setCategories, setCategoryData, setCategoryHierarchy, setCategoryLevels, setCategoryStructure)
