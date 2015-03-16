# Here are the packages that you'll need:
library(XML)
library(stringr)
library(plyr)
library(car)
# Here are the datasets you'll use:
# workingSetsList
# workingPartsList

# Let's keep to appropriate sets - after 1999
appropriateSetsAndCategories <- subset(setsAndCategories, MSRP < 150 & str_detect(Theme,"SYSTEM") & !(is.na(MSRP)) & !(is.na(Figs)) & !(is.na(Pcs)) & !(is.na(Tertiary)))
tempfit <- lm(appropriateSetsAndCategories$MSRP ~ appropriateSetsAndCategories$Pcs + appropriateSetsAndCategories$Figs + appropriateSetsAndCategories$Tertiary)
summary(tempfit)
hist(appropriateSetsAndCategories$MSRP, breaks=50)
vif(tempfit)

appropriateSetsAndCategories$PredictedMSRP <- predict(tempfit, na.action = NA)
scatterplot(appropriateSetsAndCategories$PredictedMSRP, appropriateSetsAndCategories$MSRP)
