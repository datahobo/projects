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

table(brickSets$instructionsCount)
