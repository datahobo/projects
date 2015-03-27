# let's explore the set data some, shall we?
library(ggplot2)
library(quantmod)
library(stringr)
brickSets$legoID <- paste0(brickSets$number, "-", brickSets$numberVariant)
# how many sets have prices? ----
table(brickSets$USRetailPrice)
round(table(!is.na(brickSets$USRetailPrice)) / nrow(brickSets) * 100, 0)
brickSetsPriced <- brickSets[!is.na(brickSets$USRetailPrice), ]
brickSetsPriceIds <- data.frame(brickSetsPriced$legoID)
names(brickSetsPriceIds) <- "SetID"
brickSetsPriceIds$brickset <- "brickset"
# peeron ----
table(is.na(peeronSets$MSRP))
round(table(is.na(peeronSets$MSRP)) / nrow(peeronSets) * 100, 0)
peeronSetsPriced <- peeronSets[!is.na(peeronSets$MSRP), ]
peeronSetsPriceIds <- data.frame(peeronSetsPriced$SetID)
names(peeronSetsPriceIds) <- "SetID"
peeronSetsPriceIds$peeron <- "peeron"
# now match them ----
bothPriced <- merge(brickSetsPriceIds, peeronSetsPriceIds)
table(bothPriced$peeron)
# what's our match rate?
nrow(bothPriced) / nrow(brickSetsPriceIds)
nrow(bothPriced) / nrow(peeronSetsPriceIds)
# brickset is still much better to use, even though the pricing ratio is the same.
nrow(bothPriced) - nrow(peeronSetsPriceIds)
# losing 223 sets, not bad
rm(brickSetsPriceIds, peeronSetsPriceIds, bothPriced)
# now add some data to the set list ----
# first let's get the CPI
getSymbols(Symbols = "CPIAUCSL", 
           src = 'FRED')
head(CPIAUCSL)
avg.cpi <- apply.yearly(CPIAUCSL, mean)
head(avg.cpi)
avg.cpi <- data.frame(avg.cpi)
avg.cpi$year <- as.numeric(str_sub(row.names(avg.cpi), start = 1, end = 4))
names(avg.cpi) <- c("CPI", "year")
brickSetsPriced <- merge(brickSetsPriced, avg.cpi)
rm(avg.cpi, CPIAUCSL)
brickSetsPriced$USAdjustedPrice <- brickSetsPriced$USRetailPrice / brickSetsPriced$CPI * 100
ggplot(brickSetsPriced, aes(x = CPI, y = USRetailPrice)) +
  geom_point(shape = 1)
ggplot(brickSetsPriced, aes(x = CPI, y = USAdjustedPrice)) +
  geom_point(shape = 1)
brickSetsPriced$priceAdjustment <- brickSetsPriced$USAdjustedPrice - brickSetsPriced$USRetailPrice
hist(brickSetsPriced$priceAdjustment, breaks = 1000, xlim = c(-100,100))

table(brickSetsPriced$pieces)

# explore the brickSet set list ----
tapply(brickSetsPriced$USRetailPrice, brickSetsPriced$year, mean)
# scatter the year, price, number of pieces, number owned, number wanted, number of reviews, rating
ggplot(brickSetsPriced, aes(x = year, y = USRetailPrice)) +
  geom_point(shape = 1)
ggplot(brickSetsPriced, aes(x = year, y = pieces)) +
  geom_point(shape = 1)
cor(brickSetsPriced$USRetailPrice, brickSetsPriced$pieces, use = "complete.obs")
ggplot(brickSetsPriced, aes(x = ownedByTotal, y = USRetailPrice)) +
  geom_point(shape = 1)
ggplot(brickSetsPriced, aes(x = wantedByTotal, y = USRetailPrice)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm)
ggplot(brickSetsPriced, aes(x = wantedByTotal, y = ownedByTotal)) +
  geom_point(shape = 1)


# match the brickSet and Peeron lists
names(peeronSets) <- c("legoID", "name", "theme", "year", "pieces", "minifigs",
                       "USRetailPrice", "inventory", "instructions")
peeronSets$instructions <- ifelse(peeronSets$instructions == "Yes", 1, 0)
peeronSetsPriced$instructions <- ifelse(peeronSetsPriced$instructions == "Yes", 1, 0)
names(peeronSetsPriced) <- names(peeronSets)
matchedPrices <- merge(brickSetsPriced, peeronSetsPriced, by.x = "legoID", by.y = "legoID")
matchedSets <- as.data.frame(matchedPrices$legoID)
names(matchedSets) <- c("legoID")
matchedSets$matched <- TRUE

peeronSetsPriced <- merge(peeronSetsPriced, matchedSets, all.x = TRUE)
peeronSetsPricedNoPartsList <- subset(peeronSetsPriced, is.na(peeronSetsPriced$matched))
rm(matchedSets)
# now need to merge peeronSetsPricedNoPartsList into matchedPrices ----
names(peeronSetsPricedNoPartsList) <- names(peeronSetsPriced)
temp <- names(peeronSetsPricedNoPartsList)
temp <- paste0(temp, ".y")
temp[1] <- "legoID"
temp
names(peeronSetsPricedNoPartsList) <- temp

# check for differences in the different sets
temp <- setdiff(names(matchedPrices), names(peeronSetsPricedNoPartsList))
match <- intersect(names(matchedPrices), names(peeronSetsPricedNoPartsList))
p_only <- setdiff(names(peeronSetsPricedNoPartsList), match)
names(peeronSetsPricedNoPartsList)[which(names(peeronSetsPricedNoPartsList) == "instructions.y")] <- "instructions"
names(peeronSetsPricedNoPartsList)[which(names(peeronSetsPricedNoPartsList) == "matched.y")] <- "matched"
names(peeronSetsPricedNoPartsList)[which(names(peeronSetsPricedNoPartsList) == "inventory.y")] <- "inventory"

temp <- setdiff(names(matchedPrices), names(peeronSetsPricedNoPartsList))
match <- intersect(names(matchedPrices), names(peeronSetsPricedNoPartsList))
p_only <- setdiff(names(peeronSetsPricedNoPartsList), match)
# now we can clean up the extra peeron list
rm(p_only, match, temp, missingVars, i)
peeronSetsPricedNoPartsList$matched <- NULL
# now get the difference
temp <- setdiff(names(matchedPrices), names(peeronSetsPricedNoPartsList))
# now add each of those as a column
for(i in 1:length(temp)) {
  peeronSetsPricedNoPartsList[, length(peeronSetsPricedNoPartsList) + 1] <- NA
  names(peeronSetsPricedNoPartsList)[length(peeronSetsPricedNoPartsList)] <- temp[i]  
}
# now reorder the names of peeron sets priced no parts list
peeronSetsPricedNoPartsList <- peeronSetsPricedNoPartsList[names(matchedPrices)]

pricedSets <- rbind(matchedPrices, peeronSetsPricedNoPartsList)
rm(matchedPrices, peeronSetsPricedNoPartsList, peeronSetsPriced, brickSetsPriced, temp, i)

# now that we have the full list of priced sets and the parts list, let's see how we've done.
setsWithParts <- data.frame(table(partsList$SetID))
names(setsWithParts) <- c("legoID", "partsTypesCount")

temp <- data.frame(pricedSets$legoID)
names(temp) <- "legoID"
temp$priced <- TRUE

setsWithParts <- merge(setsWithParts, temp, all.x = TRUE)
round(sum(setsWithParts$priced, na.rm = TRUE) / nrow(setsWithParts) * 100, 0)
# pretty good - 98%
rm(setsWithParts, temp)

# now let's trim down the pricedSets frame ----
pricedSets$X <- NULL
pricedSets$number <- NULL
pricedSets$numberVariant <- NULL
pricedSets$bricksetURL <- NULL
pricedSets$owned <- NULL
pricedSets$wanted <- NULL
pricedSets$qtyOwned <- NULL
pricedSets$userNotes <- NULL
pricedSets$ACMDataCount <- NULL
pricedSets$UKRetailPrice <- NULL
pricedSets$CARetailPrice <- NULL
pricedSets$EURetailPrice <- NULL
pricedSets$additionalImageCount <- NULL
pricedSets$imageFilename <- NULL
pricedSets$thumbnailURL <- NULL
pricedSets$largeThumbnailURL <- NULL
pricedSets$theme.y <- NULL
pricedSets$EAN <- NULL
pricedSets$UPC <- NULL
pricedSets$instructions2 <- NULL
pricedSets$instructions <- NULL

names(pricedSets)
str(pricedSets)
# name
pricedSets$compname <- pricedSets$name.x == pricedSets$name.y
table(pricedSets$compname)
pricedSets$compname <- NULL
# year
pricedSets$compyear <- pricedSets$year.x == pricedSets$year.y
table(pricedSets$compyear)
# pieces
pricedSets$comppcs <- pricedSets$pieces.x == pricedSets$pieces.y
table(pricedSets$comppcs)
# Convert
pricedSets$image <- as.logical(toupper(pricedSets$image))
# minifigs
pricedSets$compmfgs <- pricedSets$minifigs.x == pricedSets$minifigs.y
table(pricedSets$compmfgs)
# US Retail Price
table(!is.na(pricedSets$USRetailPrice.x))
table(pricedSets$USRetailPrice.y)
pricedSets$compprice <- pricedSets$USRetailPrice.x == pricedSets$USRetailPrice.y
table(pricedSets$compprice)
pricedSets$compprice <- NULL
str(pricedSets)
pricedSets$pricedifference <- pricedSets$USRetailPrice.x - pricedSets$USRetailPrice.y
table(pricedSets$pricedifference)
pricedSets$pricedifference <- NULL

# plot some things ----
# price and pieces
ggplot(pricedSets, aes(x = pieces.x, y = USAdjustedPrice)) +
  geom_point(shape = 1) + # use hollow circles
  geom_smooth(method = lm) # show an lm line

# now model it
legoPricing <- lm(pricedSets$USAdjustedPrice ~ pricedSets$year.x + pricedSets$theme.x + pricedSets$themeGroup + pricedSets$subtheme +
  pricedSets$pieces.x + pricedSets$minifigs.x + pricedSets$packagingType)

summary(legoPricing)
write.csv(pricedSets, "pricedSets.csv")
